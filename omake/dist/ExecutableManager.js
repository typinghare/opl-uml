"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ExecutableNameNotFoundException = exports.DuplicateExecutableNameException = exports.ExecutableManager = void 0;
const easy_app_1 = require("@typinghare/easy-app");
const Executable_1 = require("./Executable");
/**
 * Executable manager.
 */
class ExecutableManager extends easy_app_1.Manager {
    constructor() {
        super(...arguments);
        /**
         * Mapping from executable names to executable instances.
         * @private
         */
        this.byName = new Map();
    }
    /**
     * Adds an executable.
     * @param name The name of the executable (must be unique).
     * @param workingDir The working directory.
     * @param files The list of files to compile.
     * @return The executable added.
     */
    add(name, workingDir, files) {
        if (this.byName.has(name)) {
            throw new DuplicateExecutableNameException(name);
        }
        // Create an executable
        const executable = new Executable_1.Executable(this.application.getEnv());
        executable.getDatum('name').setValue(name);
        executable.getDatum('workingDir').setValue(workingDir);
        executable.getDatum('files').setValue(files);
        this.byName.set(name, executable);
        return executable;
    }
    /**
     * Returns an executable associated with the given name.
     * @param name
     */
    getByName(name) {
        const executable = this.byName.get(name);
        if (!executable) {
            throw new ExecutableNameNotFoundException(name);
        }
        return executable;
    }
}
exports.ExecutableManager = ExecutableManager;
/**
 * Thrown when a duplicate executable name is found.
 */
class DuplicateExecutableNameException extends Error {
    constructor(name) {
        super(`Duplicate executable name: ${name}`);
    }
}
exports.DuplicateExecutableNameException = DuplicateExecutableNameException;
/**
 * Thrown when an executable name does not exist.
 */
class ExecutableNameNotFoundException extends Error {
    constructor(name) {
        super(`Executable name not found: ${name}`);
    }
}
exports.ExecutableNameNotFoundException = ExecutableNameNotFoundException;
