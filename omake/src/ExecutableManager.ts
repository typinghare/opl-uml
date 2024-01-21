import { Manager } from '@typinghare/easy-app'
import { type Application } from './Application'
import { Executable } from './Executable'

/**
 * Executable manager.
 */
export class ExecutableManager extends Manager<Application> {
    /**
     * Mapping from executable names to executable instances.
     * @private
     */
    private readonly byName = new Map<string, Executable>()

    /**
     * Adds an executable.
     * @param name The name of the executable (must be unique).
     * @param workingDir The working directory.
     * @param files The list of files to compile.
     * @return The executable added.
     */
    public add(name: string, workingDir: string, files: string[]): Executable {
        if (this.byName.has(name)) {
            throw new DuplicateExecutableNameException(name)
        }

        // Create an executable
        const executable = new Executable(this.application.getEnv())
        executable.getDatum('name').setValue(name)
        executable.getDatum('workingDir').setValue(workingDir)
        executable.getDatum('files').setValue(files)

        this.byName.set(name, executable)

        return executable
    }

    /**
     * Returns an executable associated with the given name.
     * @param name
     */
    public getByName(name: string): Executable {
        const executable: Executable | undefined = this.byName.get(name)
        if (!executable) {
            throw new ExecutableNameNotFoundException(name)
        }

        return executable
    }
}

/**
 * Thrown when a duplicate executable name is found.
 */
export class DuplicateExecutableNameException extends Error {
    public constructor(name: string) {
        super(`Duplicate executable name: ${name}`)
    }
}

/**
 * Thrown when an executable name does not exist.
 */
export class ExecutableNameNotFoundException extends Error {
    public constructor(name: string) {
        super(`Executable name not found: ${name}`)
    }
}