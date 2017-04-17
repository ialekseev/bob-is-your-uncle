import {Component, VERSION} from '@angular/core'
import {Http, Headers} from '@angular/http';
import 'rxjs/Rx';

//todo: find out some decent way of using code contracts in TS
//todo: validation
//todo: configure https://github.com/fxmontigny/ng2-ace-editor
@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    styleUrls: ['app/app.css']
})
export class AppComponent {
    dirs: Array<Dir>;

    selectedDir: Dir;
    selectedSource: [Dir, Source];

    logMessages: Array<LogMessage> = [];

    showSpinner: boolean = false;

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(private http: Http) {
        console.log(`angular v${VERSION.full}`);
        this.showSpinner = true;
        http.get('sandbox/sources').map(r => r.json()).subscribe(r => {
            this.dirs = r.dirs;
            this.log("Sources have been loaded", LogMessageLevel.Info);
            this.showSpinner = false;
        });
    }

    private selectSource(dir: Dir, source: Source): void {
        this.selectedSource = [dir, source];
        this.selectedDir = null;
    }

    private selectDir(dir: Dir): void {
        this.selectedDir = dir;
        this.selectedSource = null;
    }

    private unselect() {
        this.selectedDir = null;
        this.selectedSource = null;
    }

    onSaveClick(): void {
        this.showSpinner = true;
        this.http.put('sandbox/sources', JSON.stringify({"dirs": this.dirs, "updateBuilds": false}), {headers: this.headers}).map(r => r.json()).subscribe(res => {
            this.log("Sources have been saved on the server", LogMessageLevel.Info);
            this.showSpinner = false;
        });
    }

    onDirClick(dir: Dir): void {
        this.selectDir(dir);
    }

    onSourceClick(dir: Dir, source: Source): void {
        this.selectSource(dir, source);
    }

    onAddSourceClick(dir: Dir): void {
        let newSource = new Source("new", "");
        dir.sources.push(newSource);
        this.selectSource(dir, newSource);
    }

    onRemoveSourceClick(dir: Dir, source: Source): void {
        dir.sources = dir.sources.filter(s => s.name != source.name);
        this.unselect();
    }

    onAddVariableClick(dir: Dir): void {
        dir.vars.push(new Variable("", ""));
    }

    onRemoveVariableClick(dir: Dir, variableToDelete: Variable): void {
        dir.vars = dir.vars.filter(v => v.name != variableToDelete.name)
    }

    onCheckSourceClick(dir: Dir, source: Source): void {
        this.showSpinner = true;
        this.http.post('sandbox/sources/build', JSON.stringify({"content": source.content, "vars": dir.vars}), {headers: this.headers}).map(r => r.json()).subscribe(res => {
            if (res.errors) {
                let error = (res.errors as Array<BuildError>)[0];
                this.log(`(${error.startCoordinates.x},${error.startCoordinates.y}): ${error.message}`, LogMessageLevel.Error)
            } else {
                this.log("Successfully checked: "  + dir.path + "/" + source.name, LogMessageLevel.Success)
            }
            this.showSpinner = false;
        });
    }

    isActiveDir(dir: Dir): boolean {
        return this.selectedDir != null && this.selectedDir.path == dir.path;
    }

    isActiveSource(source: Source): boolean {
        return this.selectedSource != null && this.selectedSource[1].name == source.name;
    }

    log(text: string, level: LogMessageLevel): void {
        this.logMessages.unshift(new LogMessage(new Date(), text, level));
    }

    isLogMessageErrorLevel(level: LogMessageLevel): boolean {
        return level == LogMessageLevel.Error;
    }

    isLogMessageWarningLevel(level: LogMessageLevel): boolean {
        return level == LogMessageLevel.Warning;
    }

    isLogMessageInfoLevel(level: LogMessageLevel): boolean {
        return level == LogMessageLevel.Info;
    }

    isLogMessageSuccessLevel(level: LogMessageLevel): boolean {
        return level == LogMessageLevel.Success;
    }
}

export class Variable {
    constructor(public name: string, public value: string) {}
}

export class Source {
    constructor(public name: string, public content: string) {}
}

export class Dir {
    constructor(public path: string, public sources: Array<Source>, public vars: Array<Variable>) {}
}

export class ErrorCoordinates {
    constructor(public x: number, public y: number) {}
}

export class BuildError {
    constructor(public startOffset: number, public endOffset: number, public startCoordinates: ErrorCoordinates, public endCoordinates: ErrorCoordinates, public message: string) {}
}

export enum LogMessageLevel {Error, Warning, Info, Success}

export class LogMessage {
    constructor(public timestamp: Date, public text: string, public level: LogMessageLevel ) {}
}