import {Component} from 'angular2/core';
import {Http, Headers, HTTP_PROVIDERS} from 'angular2/http'
import 'rxjs/Rx'

@Component({
    selector: 'sandbox-app',
    templateUrl: 'app/app.html' ,
    providers: [HTTP_PROVIDERS] //todo: remove?
})
export class App {
    http: Http
    dir: string;
    sources: Array<string>;
    variables: Array<Variable>;

    selectedSourceFilePath: string;
    selectedSourceContent: string;

    variablesSaved = false;
    sourceSaved = false;

    private headers = new Headers({'Content-Type': 'application/json'});

    constructor(http: Http) {
        this.http = http;
        http.get('sandbox/sources').map(r => JSON.parse(r._body)).subscribe(sources => {
            this.dir = sources.dir;
            this.sources = sources.list;
            this.variables = sources.vars;
        });
    }

    onSourceClick(source: string): void {
        this.http.get(encodeURI('sandbox/sources/' + source)).map(r => JSON.parse(r._body)).subscribe(source => {
            this.selectedSourceFilePath = source.filePath;
            this.selectedSourceContent = source.content;
        });
    }

    onSaveSelectedSourceClick(): void {
        this.http.put(encodeURI('sandbox/sources/' + this.selectedSourceFilePath), JSON.stringify({"content": this.selectedSourceContent}), {headers: this.headers}).subscribe(res => {
            this.sourceSaved = true;
            setTimeout(function() {this.sourceSaved = false;}.bind(this), 500); //todo: refactor
        });
    }

    onSaveVariablesClick(): void {
        this.variables = this.variables.filter(v => v.name);
        this.http.put(encodeURI('sandbox/sources/vars/' + this.dir), JSON.stringify({"vars": this.variables}), {headers: this.headers}).subscribe(res => {
            this.variablesSaved = true;
            setTimeout(function() {this.variablesSaved = false;}.bind(this), 500); //todo: refactor
        });
    }

    onAddVariableClick(): void {
        this.variables.push(new Variable("", ""));
    }

    onRemoveVariableClick(variableToDelete: Variable): void {
        this.variables = this.variables.filter(v => v.name != variableToDelete.name)
    }
}

export class Variable {
    name: string;
    value: string;
    constructor(n: string, v: string) { this.name = n; this.value = v; }
}