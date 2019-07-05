use rand::Rng;
use std::path::Path;

fn superstring_to_string(tiles: &[scrabble::Tile]) -> String {
    let mut s = String::new();
    s.reserve(tiles.len());
    for &t in tiles {
        s.push(t.as_char());
    }
    s
}

#[derive(Debug)]
struct GeneticAlgorithmOptions {
    population_size: usize,
    generation_survival_rate: f32,
    max_generation_count: Option<usize>,
    stagnation_limit: usize,
    keep_best: bool,
    max_mutation_count: usize,
    max_mutation_len: usize,
    string_seeds: Vec<String>,
}

impl Default for GeneticAlgorithmOptions {
    fn default() -> Self {
        GeneticAlgorithmOptions {
            population_size: 10_000,
            generation_survival_rate: 0.05,
            max_generation_count: None,
            stagnation_limit: 100,
            keep_best: false,
            max_mutation_count: 3,
            max_mutation_len: 5,
            string_seeds: Vec::new(),
        }
    }
}

fn mutate_superstring<R: Rng>(
    rng: &mut R,
    tiles: &[scrabble::Tile],
    _options: &GeneticAlgorithmOptions,
) -> Vec<scrabble::Tile> {
    // Big rotation:
    if rng.gen_bool(0.25) {
        let mut new_tiles = tiles.to_vec();
        let mid = rng.gen_range(1, tiles.len() - 1);
        new_tiles.rotate_left(mid);
        return new_tiles;
    }

    // Cut-and-splice:
    if rng.gen_bool(0.5) {
        let left;
        let right;
        loop {
            let a = rng.gen_range(0, tiles.len());
            let b = rng.gen_range(0, tiles.len());
            if a + 1 < b {
                left = a;
                right = b;
                break;
            }
            if b + 1 < a {
                left = b;
                right = a;
                break;
            }
        }

        let len = rng.gen_range(1, right - left);

        let mut new_tiles = tiles.to_vec();
        &mut new_tiles[left..right].rotate_left(len);

        debug_assert_eq!(new_tiles.len(), tiles.len());

        return new_tiles;
    }

    // Swap two tiles:
    let mut new_tiles = tiles.to_vec();
    let a = rng.gen_range(0, tiles.len());
    let b = {
        let mut b;
        loop {
            b = rng.gen_range(0, tiles.len());
            if a != b {
                break;
            }
        }
        b
    };
    new_tiles.swap(a, b);
    new_tiles
}

fn run_genetic_algorithm<R: Rng>(
    log: &mut std::io::Write,
    rng: &mut R,
    dict: &scrabble::Dictionary,
    options: &GeneticAlgorithmOptions,
) -> Result<(), Box<dyn std::error::Error>> {
    //
    writeln!(log, "Starting new run (options={:?})", options)?;
    let mut population = Vec::new();
    population.reserve(options.population_size);
    if !options.string_seeds.is_empty() {
        // This is a hack job.
        use rand::seq::SliceRandom;
        let mut tile_counter = scrabble::TileCounter::new();
        for s in &options.string_seeds {
            tile_counter.count_tiles_in_str(s);
        }
        tile_counter.invert();
        let mut extra_tiles = Vec::new();
        for (t, n) in tile_counter.tile_counts() {
            assert!(0 <= n);
            extra_tiles.resize(extra_tiles.len() + n as usize, t);
        }
        let mut string_seeds = options
            .string_seeds
            .iter()
            .map(|s| {
                s.chars()
                    .map(|ch| scrabble::Tile::new(ch))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let seed_tile_count =
            string_seeds.iter().fold(0, |sum, s| sum + s.len());
        debug_assert_eq!(
            seed_tile_count + extra_tiles.len(),
            scrabble::TILE_COUNT
        );
        for _i in 0..options.population_size {
            extra_tiles.shuffle(rng);
            let mut extra_index = 0;
            string_seeds.shuffle(rng);
            let mut next_string = Vec::new();
            next_string.reserve(scrabble::TILE_COUNT);
            for (j, seed) in string_seeds.iter().enumerate() {
                let remaining_seeds = string_seeds.len() - j;
                let remaining_extras = extra_tiles.len() - extra_index;
                let n = extra_index + remaining_extras / remaining_seeds;
                next_string.extend_from_slice(&extra_tiles[extra_index..n]);
                extra_index = n;
                next_string.extend_from_slice(seed);
            }
            next_string.extend_from_slice(&extra_tiles[extra_index..]);
            debug_assert_eq!(next_string.len(), scrabble::TILE_COUNT);
            population.push(next_string);
        }
    } else {
        for _i in 0..options.population_size {
            population.push(scrabble::Tile::draw_bag(rng));
        }
    }

    /*
    for s in &population {
        let mut t = scrabble::TileCounter::new();
        t.count_tiles_in_str(&superstring_to_string(s));
        t.invert();
        assert!(
            t.tile_counts().all(|(t, n)| n == 0),
            "bad string {:?}",
            superstring_to_string(s)
        );
    }
    */

    let mut next_population = Vec::new();
    next_population.reserve(options.population_size);

    let generation_survival_count = (options.population_size as f64
        * options.generation_survival_rate as f64)
        as usize;

    let mut finder = scrabble::ContiguousWordFinder::new(&dict);

    let mut best = (0, Vec::new());
    let mut generation_count = 0;
    let mut stagnation_history = vec![0];
    loop {
        generation_count += 1;
        match options.max_generation_count {
            Some(n) if n == generation_count => break,
            _ => {}
        }
        writeln!(
            log,
            "Running generation {} (stagnation_count={}).",
            generation_count,
            stagnation_history.len()
        )?;
        let mut results = Vec::new();
        results.reserve(population.len());
        for (i, individual) in population.iter().enumerate() {
            let score = finder.find_contiguous_words(individual, false);
            results.push((score, i));
        }
        results.sort_by(|a, b| {
            if a > b {
                std::cmp::Ordering::Less
            } else if a == b {
                std::cmp::Ordering::Equal
            } else {
                std::cmp::Ordering::Greater
            }
        });

        // Check best, including stagnation.
        let &(score, superstring_index) = results.first().unwrap();
        let superstring = &population[superstring_index];
        writeln!(
            log,
            "  Best superstring  : {} (score={}).",
            superstring_to_string(superstring),
            score,
        )?;
        if score > best.0 {
            best = (score, superstring.clone());
        }
        let n = stagnation_history
            .iter()
            .take_while(|&&n| n < score)
            .count();
        stagnation_history.drain(0..n);
        stagnation_history.push(score);
        if stagnation_history.len() >= options.stagnation_limit {
            break;
        }

        // Check worst.
        let &(score, superstring_index) = results.last().unwrap();
        let superstring = &population[superstring_index];
        writeln!(
            log,
            "  Worst superstring : {} (score={}).",
            superstring_to_string(superstring),
            score,
        )?;

        // Mutate next generation.
        next_population.clear();
        for (i, &(_score, superstring_index)) in
            results.iter().take(generation_survival_count).enumerate()
        {
            let superstring = &population[superstring_index];
            if options.keep_best {
                next_population.push(superstring.clone()); // parent survives
            }
            let fill_len = if i + 1 >= generation_survival_count {
                options.population_size
            } else {
                (options.population_size as f64
                    * (i as f64 / generation_survival_count as f64))
                    as usize
            };
            while next_population.len() < fill_len {
                let mutant = mutate_superstring(rng, superstring, options);
                next_population.push(mutant);
            }
        }
        std::mem::swap(&mut population, &mut next_population);
    }

    writeln!(
        log,
        "Best superstring: {} (score={}).",
        superstring_to_string(&best.1),
        best.0
    )?;

    Ok(())
}

fn main() -> Result<(), Box<std::error::Error>> {
    use rand::SeedableRng;

    let matches = clap::App::new("riddler_2019_06_28")
        .author(clap::crate_authors!("\n"))
        .about("Solves for the fivethirtyeight.com Riddler 2019-06-28.")
        .subcommand(
            clap::SubCommand::with_name("mutate")
                .about("Mutates the given string")
                .arg(
                    clap::Arg::with_name("rng_seed")
                        .long("rng-seed")
                        .value_name("N")
                        .validator(|s| {
                            u64::from_str_radix(&s, 10)
                                .map(|_| ())
                                .map_err(|e| e.to_string())
                        }),
                )
                .arg(clap::Arg::with_name("string").required(true)),
        )
        .subcommand(
            clap::SubCommand::with_name("repl")
                .about("Prints stats about string read from stdin"),
        )
        .subcommand(
            clap::SubCommand::with_name("run")
                .about("Runs the genetic algorithm to find high-score string")
                .arg(
                    clap::Arg::with_name("rng_seed")
                        .long("rng-seed")
                        .value_name("N")
                        .validator(|s| {
                            u64::from_str_radix(&s, 10)
                                .map(|_| ())
                                .map_err(|e| e.to_string())
                        }),
                )
                .arg(
                    clap::Arg::with_name("string_seed")
                        .long("string-seed")
                        .value_name("STR")
                        .multiple(true)
                        .use_delimiter(true)
                        .validator(|s| {
                            for ch in s.chars() {
                                if !scrabble::Tile::is_valid_char(ch) {
                                    return Err(format!(
                                        "string seed char {:?} is invalid",
                                        ch
                                    ));
                                }
                            }
                            let mut tile_counter = scrabble::TileCounter::new();
                            tile_counter.count_tiles_in_str(&s);
                            tile_counter.invert();
                            for (t, n) in tile_counter.tile_counts() {
                                if n < 0 {
                                    return Err(format!(
                                        "string seed char {:?} is used too many times",
                                        t.as_char()));
                                }
                            }
                            Ok(())
                        }),
                ),
        )
        .subcommand(
            clap::SubCommand::with_name("tile-count")
                .about(
                    "Counts the number of each tile used in the given strings",
                )
                .arg(clap::Arg::with_name("invert").long("invert"))
                .arg(
                    clap::Arg::with_name("string")
                        .required(true)
                        .multiple(true)
                        .validator(|s| {
                            for ch in s.chars() {
                                if !scrabble::Letter::is_valid_char(ch) {
                                    return Err(format!(
                                        "string char {:?} is invalid",
                                        ch
                                    ));
                                }
                            }
                            Ok(())
                        }),
                ),
        )
        .get_matches();

    let dict = scrabble::Dictionary::load_from_file(Path::new(
        "/Users/cbrandenburg/Downloads/dict/enable1.txt",
    ))?;

    if let Some(m) = matches.subcommand_matches("mutate") {
        let rng_seed = match m.value_of("rng_seed") {
            Some(s) => u64::from_str_radix(s, 10).unwrap(),
            None => 0,
        };
        let mut rng = rand::rngs::StdRng::seed_from_u64(rng_seed);
        let tiles = m
            .value_of("string")
            .unwrap()
            .chars()
            .map(|ch| scrabble::Tile::new(ch))
            .collect::<Vec<_>>();
        let options = GeneticAlgorithmOptions::default();
        let mutant = mutate_superstring(&mut rng, &tiles, &options);
        println!("{}", superstring_to_string(&mutant));
        return Ok(());
    }

    if let Some(_) = matches.subcommand_matches("repl") {
        use std::io::BufRead;
        let mut finder = scrabble::ContiguousWordFinder::new(&dict);
        let r = std::io::BufReader::new(std::io::stdin());
        for line_result in r.lines() {
            match line_result {
                Err(e) => return Err(Box::new(e)),
                Ok(ref line) => {
                    let s = line
                        .chars()
                        .map(|ch| scrabble::Tile::new(ch))
                        .collect::<Vec<_>>();
                    let score = finder.find_contiguous_words(&s, false);
                    println!("score: {}", score);
                }
            }
        }
        return Ok(());
    }

    if let Some(m) = matches.subcommand_matches("run") {
        let rng_seed = match m.value_of("rng_seed") {
            Some(s) => u64::from_str_radix(s, 10).unwrap(),
            None => 0,
        };

        let mut rng = rand::rngs::StdRng::seed_from_u64(rng_seed);

        let mut options = GeneticAlgorithmOptions::default();
        if let Some(string_seeds) = m.values_of("string_seed") {
            options.string_seeds =
                string_seeds.into_iter().map(|s| String::from(s)).collect();
        }

        let mut log = std::io::stderr();
        run_genetic_algorithm(&mut log, &mut rng, &dict, &options)?;

        return Ok(());
    }

    if let Some(m) = matches.subcommand_matches("tile-count") {
        let mut tile_counter = scrabble::TileCounter::new();
        for word in m.values_of("string").unwrap() {
            tile_counter.count_tiles_in_str(word);
        }
        if m.is_present("invert") {
            tile_counter.invert();
        }
        for lt in scrabble::Letter::all_letters() {
            let ch = lt.as_char();
            println!("{} : {}", ch, tile_counter.char_count(ch));
        }

        return Ok(());
    }

    unreachable!();
}
