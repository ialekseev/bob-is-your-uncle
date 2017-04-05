import {Component} from 'angular2/core';
import {Http, Headers, HTTP_PROVIDERS} from 'angular2/http'
import 'rxjs/Rx'

//todo: find out some decent way of using code contracts in TS
//todo: validation
@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    styleUrls: ['app/app.css'],
    providers: [HTTP_PROVIDERS] //todo: remove?
})
export class App {
    http: Http;

    dirs: Array<Dir>;

    selectedDir: Dir;
    selectedSource: [Dir, Source];

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(http: Http) {
        this.http = http;
        http.get('sandbox/sources').map(r => r.json()).subscribe(r => {
            this.dirs = r.dirs;
        });
    }

    private selectSource(dir: Dir, source: Source): void {
        this.selectedSource = [dir, source];
        this.selectedDir = null;
    }

    private selectDir(dir: Dir) {
        this.selectedDir = dir;
        this.selectedSource = null;
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

    onAddVariableClick(dir: Dir): void {
        dir.vars.push(new Variable("", ""));
    }

    onRemoveVariableClick(dir: Dir, variableToDelete: Variable): void {
        dir.vars = dir.vars.filter(v => v.name != variableToDelete.name)
    }

    private flash(set: (v: boolean) => void) {
        set(true);
        setTimeout(function() {set(false);}.bind(this), 500);
    }
}

export class Variable {
    name: string;
    value: string;
    constructor(n: string, v: string) { this.name = n; this.value = v; }
}

export class Source {
    name: string;
    content: string;
    constructor(n: string, c: string) { this.name = n; this.content = c; }
}

export class Dir {
    path: string;
    sources: Array<Source>;
    vars: Array<Variable>;
    constructor(p: string, s: Array<Source>, v: Array<Variable>) { this.path = p; this.sources = s; this.vars = v; }
}