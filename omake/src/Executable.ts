import { DataCollection, Datum } from '@typinghare/extrum'
import { Environment } from './Environment'
import * as child_process from 'child_process'
import { exec } from 'child_process'
import process from 'process'
import path from 'path'
import * as fs from 'fs'

/**
 * Executable.
 */
export class Executable extends DataCollection<ExecutableData> {
    /**
     * Creates an executable.
     * @param env The environment where this executable will run.
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
     * Adds all module files (.ml files) in the working directory.
     */
    public addAllFilesInWorkingDir(): void {
        const workingDir: string = this.getValue('workingDir')
        const files: string[] = []
        for (const file of workingDir) {
            const filePath: string = path.join(workingDir, file)
            const stat = fs.statSync(filePath)

            if (stat.isFile()) {
                files.push(filePath)
            }
        }

        this.getDatum('files').setValue(files)
    }

    /**
     * Runs this executable.
     */
    public run(): void {
        // Convert files' relative paths to absolute paths
        const workingDir: string = this.env.getValue('workingDir')
        const absoluteWorkingDir: string = path.join(workingDir, this.getValue('workingDir'))
        const getAbsoluteFilepath = (file: string) => path.join(absoluteWorkingDir, file).trim()
        const files: string[] = this.getValue('files').map(getAbsoluteFilepath)

        // Check if all files exist
        files.forEach(this.checkFile)

        // Compile files
        const outputDir: string = this.env.getValue('outputDir')
        const targetFilepath: string = path.join(outputDir, this.getValue('name'))
        const command: string = `ocamlc -o ${targetFilepath} ${files.join(' ')}`
        exec(command, (error, stdout, stderr) => {
            if (error && this.checkExternalError(error, stdout, stderr)) {
                return
            }

            // Execute the built file
            this.executeFile(targetFilepath)

            // Remove all intermediate compiled files
            this.removeIntermediateFiles(files)
        })
    }

    /**
     * Checks whether file exists.
     * @param file The file to check.
     * @private
     */
    private checkFile(file: string): void {
        if (!fs.existsSync(file)) {
            throw new FileNotFoundException(file)
        }
    }

    /**
     * Logs error information.
     * @param error The error encountered.
     * @param stderr The standard error output string.
     * @private
     */
    private logError(error: child_process.ExecException, stderr: string): void {
        if (error) {
            console.log(`An internal exception is encountered: `)
            console.error(error.message)
            console.log(`\nError output:`)
            console.error(stderr)
            return
        }
    }

    /**
     * Executes an executable file.
     * @param targetFilepath The path of the executable file.
     * @private
     */
    private executeFile(targetFilepath: string) {
        exec(targetFilepath, (error, stdout, stderr) => {
            if (error && this.checkExternalError(error, stdout, stderr)) {
                return
            }

            // This prevents the trailing '%' of the output
            if (!stdout.endsWith('\n')) {
                stdout += '\n'
            }

            process.stdout.write(stdout, 'utf-8')
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

    /**
     * Logs an external error (if exists)
     * @param error The error encountered.
     * @param stdout The standard output string.
     * @param stderr The standard error output string.
     * @return false if the external error exists; true otherwise.
     * @private
     */
    private checkExternalError(
        error: child_process.ExecException,
        stdout: string,
        stderr: string,
    ): boolean {
        if (error) {
            this.logError(error, stderr)
            if (error.code !== 0) {
                console.log(`Fail to execute [ ${this.getValue('name')} ]: `)
                console.log(stdout)
            }

            return true
        }

        return false
    }
}

export interface ExecutableData {
    // The name of the executable
    name: string

    // The working directory
    workingDir: string

    // The OCaml files (modules) to compile
    files: string[]
}

export class FileNotFoundException extends Error {
    public constructor(filepath: string) {
        super(`File not found: ${filepath}`)
    }
}