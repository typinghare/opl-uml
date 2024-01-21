import { Environment, Executable, ExecutableManager } from './ocaml'
import path from 'path'
import * as fs from 'fs'

const environment: Environment = new Environment(
    // <root>
    path.join(__dirname, '..'),

    // <root>/script/target
    path.join(__dirname, 'target'),
)

const executableManger = new ExecutableManager(environment)

const ocamlMakeFilepath: string = path.join(environment.workingDir, 'OCamlMake.ini')
const lines: string[] = fs.readFileSync(ocamlMakeFilepath).toString('utf-8').split('\n')
lines.forEach((line: string) => {
    line = line.trim()

    // Comment line
    if (line.startsWith('#')) return

    const sp: string[] = line.split('=')
    if (sp.length != 2) return

    const [name, rhs] = sp
    const [workingDir, ...files] = rhs.trim().split(' ')
    executableManger.add(name.trim(), workingDir, files.map((file: string) => file.trim()))
})

export const executables: Executable[] = executableManger.getExecutables()