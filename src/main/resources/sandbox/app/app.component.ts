import {Component, VERSION, ViewChild} from '@angular/core'
import {Http, Response} from '@angular/http';
import 'rxjs/Rx';
import {ContextMenuComponent } from 'angular2-contextmenu';

//todo: find out some decent way of using code contracts in TS
//todo: validation
@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    styleUrls: ['app/app.css']
})
export class AppComponent {
    http: Http;

    dirs: Array<Dir>;

    selectedDir: Dir;
    selectedSource: [Dir, Source];

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(http: Http) {
        console.log(`angular v${VERSION.full}`);
        http.get('sandbox/sources').map(r => r.json()).subscribe(r => {
            this.dirs = r.dirs;
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