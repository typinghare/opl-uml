"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Environment = void 0;
const extrum_1 = require("@typinghare/extrum");
/**
 * Environment.
 */
class Environment extends extrum_1.DataCollection {
    constructor() {
        super({
            workingDir: extrum_1.Datum.of(''),
            outputDir: extrum_1.Datum.of(''),
        });
    }
}
exports.Environment = Environment;
