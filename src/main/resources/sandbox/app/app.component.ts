import {Component, VERSION} from '@angular/core'
import {Http, Headers} from '@angular/http';
import 'rxjs/Rx';
//import {InlineEditorDirectives} from 'ng2-inline-editor';

//todo: find out some decent way of using code contracts in TS
//todo: validation
//todo: configure https://github.com/fxmontigny/ng2-ace-editor
//todo: set editor.$blockScrolling from the component. Is the problem here(https://github.com/fxmontigny/ng2-ace-editor/issues/12) due to "ngIf" directive?
@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    styleUrls: ['app/app.css']
})
export class AppComponent {
    dirs: Array<Dir>;

    selectedDir: Dir;
    selectedSource: [Dir, Source];

    lastCheckError: BuildError;

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(private http: Http) {
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

    onCheckSourceClick(dir: Dir, source: Source): void {
        this.lastCheckError = null;
        this.http.post('sandbox/sources/build', JSON.stringify({"content": source.content, "vars": dir.vars}), {headers: this.headers}).map(r => r.json()).subscribe(res => {
            console.log(res);
            if (res.errors) {
                this.lastCheckError = (res.errors as Array<BuildError>)[0];
            }
        });
    }

    isActiveDir(dir: Dir): boolean {
        return this.selectedDir != null && this.selectedDir.path == dir.path;
    }

    isActiveSource(source: Source): boolean {
        return this.selectedSource != null && this.selectedSource[1].name == source.name;
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