import {Component} from 'angular2/core';
import {Http, Headers, HTTP_PROVIDERS} from 'angular2/http'
import 'rxjs/Rx'

@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    styleUrls: ['app/app.css'],
    providers: [HTTP_PROVIDERS] //todo: remove?
})
export class App {
    http: Http
    dir: string;
    sources: Array<Source>;
    variables: Array<Variable>;

    selectedSourceIndex: number | null = null;

    variablesSaved = false;
    sourceSaved = false;

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(http: Http) {
        this.http = http;
        http.get('sandbox/sources').map(r => r.json()).subscribe(sources => {
            this.dir = sources.dir;
            this.sources = (sources.list as Array<string>).map(s => new Source(s, ""))
            this.variables = sources.vars;
        });
    }

    onSourceClick(index: number): void {
        let filePath = this.sources[index].filePath;
        this.http.get(encodeURI('sandbox/sources/' + filePath)).map(r => r.json()).subscribe(res => {
            this.selectedSourceIndex = index;
            this.sources[index] = new Source(res.filePath, res.content)
            console.log(res);
         });
    }

    onAddSourceClick(): void {
        //todo: implement
        //this.sources.push(new Variable("", ""));
    }

    onSaveSelectedSourceClick(): void {
        let filePath = this.sources[this.selectedSourceIndex].filePath;
        let content = this.sources[this.selectedSourceIndex].content;

        this.http.put(encodeURI('sandbox/sources/' + filePath), JSON.stringify({"content": content}), {headers: this.headers}).subscribe(res => {
            this.flash(a => this.sourceSaved = a);
        });
    }

    onSaveVariablesClick(): void {
        this.variables = this.variables.filter(v => v.name);
        this.http.put(encodeURI('sandbox/sources/vars/' + this.dir), JSON.stringify({"vars": this.variables}), {headers: this.headers}).subscribe(res => {
            this.flash(a => this.variablesSaved = a);
        });
    }

    onAddVariableClick(): void {
        this.variables.push(new Variable("", ""));
    }

    onRemoveVariableClick(variableToDelete: Variable): void {
        this.variables = this.variables.filter(v => v.name != variableToDelete.name)
    }

    private flash(set: (v: boolean) => void) {
        set(true);
        setTimeout(function() {set(false);}.bind(this), 500);
    }
}

export class Source {
    filePath: string;
    content: string;
    constructor(f: string, c: string) { this.filePath = f; this.content = c; }
}

export class Variable {
    name: string;
    value: string;
    constructor(n: string, v: string) { this.name = n; this.value = v; }
}