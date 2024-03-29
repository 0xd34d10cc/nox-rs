import sys
import os

def print_lines(text, indent):
    lines = text.split('\n')

    if len(lines) == 1:
        print(' ' * indent + f'"{lines[0]}"')
        return

    if len(text.strip()) == 0:
        print(' ' * indent, '""')
        return

    print(' ' * indent + '"')
    for line in text.split('\n'):
        if len(line) == 0:
            continue
        print(' ' * indent + f'{line}')
    print(' ' * indent + '"')

def main():
    if len(sys.argv) != 2:
        print('Usage: statements.py <dir>')

    base = sys.argv[1]

    cases = []
    for entry in os.listdir(base):
        if not os.path.isfile(os.path.join(base, entry)):
            continue

        name, ext = os.path.splitext(os.path.join(base, entry))
        if ext != '.expr':
            continue

        try:
            program = open(os.path.join(base, entry), 'rt').read()
            stdin = open(os.path.join(name + '.input'), 'rt').read()
            stdout = open(os.path.join(base, 'orig', os.path.basename(name) + '.log')).read()

            reads = stdout.count('>')
            stdin = [int(line) for line in stdin.split('\n') if len(line) != 0]
            stdout = [int(line) for line in stdout.replace('>', '').split('\n') if len(line) != 0]

            cases.append({
                'name': os.path.basename(name),
                'program': program,
                'stdin': stdin,
                'stdout': stdout,
                'reads': reads
            })
        except Exception as e:
            print(f'Skipping {entry} due to exception: {e}')
            continue

    print('// This file was generated by statements.py\n')
    print('use super::run;')
    for case in cases:
        name = case['name']
        program = case['program']
        inputs = ', '.join(str(n) for n in case['stdin'])
        outputs = ', '.join(str(n) for n in case['stdout'])
        reads = case['reads']

        print('#[test]')
        print(f'fn {name}() {{')
        print(f'    run(')
        print_lines(program, indent=8)
        print('         ,')
        print(f'        &[{inputs}],')
        print(f'        &[{outputs}],')
        print(f'        {reads}')
        print('    );')

        print(f'}}\n')


if __name__ == '__main__':
    main()