use eval::{eval, Namespace, Environment};
use parser::parse;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn repl() {
    let mut ns = Namespace::new("user".to_owned());
    let mut env = Environment::empty();
    let mut binding_count = 0;

    let mut rl = Editor::new();
    if let Err(_) = rl.load_history("~/.lambada_history") {
    }
    println!("lambada 0.1-alpha (press CTRL+D to quit)\n");
    loop {
        let readline = rl.readline("lmbd> ");
        match readline {
            Ok(line) => {
                let parsed = parse(line.clone()).unwrap();
                match eval(&mut ns, &env, &parsed) {
                    Ok(val) => {
                        let binding = format!("${}", binding_count);
                        println!("{} = {}", binding, &val);
                        env = env.define(binding, val);
                        binding_count = binding_count + 1;
                        rl.add_history_entry(&line);
                    },
                    Err(e) => {
                        println!("Error: {}", e);
                    }
                }
                rl.add_history_entry(&line);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("~/.lambada_history").unwrap();
}
