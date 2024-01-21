import * as process from 'process'
import { executables } from './executables'
import { Executable } from './ocaml'

const args: string[] = process.argv.slice(2)
const name = args[0]

const predicate = (executable: Executable) => executable.getValue('name') == name
const executable = executables.find(predicate)

if (executable) {
    executable.run()
} else {
    throw new Error(`Executable not found: ${name}`)
}