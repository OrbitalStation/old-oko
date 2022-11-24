use oko::*;

fn main() {
    let code = remove_comments(std::fs::read_to_string("code").unwrap().replace("    ", "\t"));

    let mut stmts = parse_raw_oko_code(&code).unwrap();

    check_each_function_is_unique_and_collect_overloads_into_one(&mut stmts);

    parse_body_in_each_function(&mut stmts);

    bake_types();

    println!("{stmts:#?}");
}
