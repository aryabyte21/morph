fn greet(name: &str) {
    println!("hi {}", name);
    if name.len() > 0 {
        println!("{}", name);
    }
    log::info!("done");
}

fn main() {
    greet("world");
    println!("end");
}
