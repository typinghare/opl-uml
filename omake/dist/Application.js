"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Application = void 0;
const easy_app_1 = require("@typinghare/easy-app");
const ExecutableManager_1 = require("./ExecutableManager");
/**
 * Application.
 */
class Application extends easy_app_1.EasyApplication {
    constructor(env) {
        super([
            ExecutableManager_1.ExecutableManager,
        ]);
        this.env = env;
    }
    /**
     * Returns the environment.
     */
    getEnv() {
        return this.env;
    }
}
exports.Application = Application;
