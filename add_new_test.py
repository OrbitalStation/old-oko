import subprocess
from sys import argv
from pathlib import Path

src = Path('code').open('r').read()

result = subprocess.run(["cargo", "run", "--quiet"], capture_output=True)
result = result.stdout.decode('utf-8') + result.stderr.decode('utf-8')

print(result)

assert len(argv) == 2

test_name = argv[1]

test_path = Path('tests') / (test_name + '.rs')

code = f"""mod common;

const SRC: &str = r#"
{src}
"#;

const RESULT: &str = r#"
{result}
"#;

#[test]
fn test() {{ common::test_single_file(SRC, RESULT) }}
"""

test_path.open('w').write(code)

stderr = subprocess.run(["cargo", "test", "-q", test_name], capture_output=True).stderr.decode('utf-8')

if stderr.find('FAILED') == -1:
    print('------ TEST SUCCESSFULLY PASSED ------')
else:
    print('------ TEST FAILED ------')
