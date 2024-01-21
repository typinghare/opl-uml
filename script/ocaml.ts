import * as path from 'path'
import { exec } from 'child_process'
import { DataCollection, Datum } from '@typinghare/extrum'
import * as process from 'process'

/**
 * The environment.
 */
export class Environment {
    /**
     * Creates an environment.
     * @param workingDir The working directory.
     * @param outputDir The output directory.
     */
    public constructor(
        public readonly workingDir: string,
        public readonly outputDir: string,
    ) {
    }
}

/**
 * An OCaml executable
 */
export class Executable extends DataCollection<ExecutableData> {
    /**
     * Creates an executable.
     * @param env The environment.
     */
    public constructor(
        private readonly env: Environment,
    ) {
        super({
            name: Datum.of(''),
            workingDir: Datum.of(''),
            files: Datum.of([]),
        })
    }

    /**
     * Runs this executable.
     */
    public run(): void {
        // Files
        const absoluteWorkingDir: string = path.join(this.env.workingDir, this.getValue('workingDir'))
        const filepathMapper = (file: string) => path.join(absoluteWorkingDir, file)
        const files: string[] = this.getValue('files').map(filepathMapper)

        // Compile files
        const targetFilepath: string = path.join(this.env.outputDir, this.getValue('name'))
        const command: string = `ocamlc -o ${targetFilepath} ${files.join(' ')}`
        exec(command, () => {
            this.executeFile(targetFilepath)
            this.removeIntermediateFiles(files)
        })
    }

    /**
     * Executes an executable file.
     * @param targetFilepath The path of the executable file.
     * @private
     */
    private executeFile(targetFilepath: string) {
        exec(targetFilepath, (error, stdout, stderr) => {
            if (error) {
                console.error(`exec error: ${error}`)
                console.error(stderr)
                return
            }

            process.stdout.write(stdout)
        })
    }

    /**
     * Removes .cmi(compiled interface file) and .cmo(compiled object file).
     * @private
     */
    private removeIntermediateFiles(files: string[]) {
        const dirSet = new Set()
        files.forEach((file: string) => dirSet.add(path.dirname(file)))
        const dirString: string = Array.from(dirSet).join(' ')

        exec(`find ${dirString} -name "*.cmi" -delete`)
        exec(`find ${dirString} -name "*.cmo" -delete`)
    }
}

/**
 * Executable manager.
 */
export class ExecutableManager {
    private readonly byName = new Map<string, Executable>()

    /**
     * Creates an executable manager.
     * @param env The environment.
     */
    public constructor(private readonly env: Environment) {
    }

    /**
     * Adds an executable.
     * @param name The name of the executable (must be unique).
     * @param workingDir The working directory.
     * @param files The list of files to compile.
     */
    public add(name: string, workingDir: string, files: string[]): void {
        if (this.byName.has(name)) {
            throw new Error(`Duplicate executable name: ${name}`)
        }

        // Default file: main.ml
        if (files.length === 0) {
            files.push('main.ml')
        }

        const executable = new Executable(this.env)
        executable.getDatum('name').setValue(name)
        executable.getDatum('workingDir').setValue(workingDir)
        executable.getDatum('files').setValue(files)

        this.byName.set(name, executable)
    }

    /**
     * Returns all executables.
     */
    public getExecutables(): Executable[] {
        return Array.from(this.byName.values())
    }
}

/**
 * Executable data.
 */
export interface ExecutableData {
    // The name of the executable
    name: string

    // The working directory
    workingDir: string

    // The OCaml files to compile
    files: string[]
}