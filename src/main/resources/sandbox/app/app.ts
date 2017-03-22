import {Component} from 'angular2/core';

@Component({
  selector: 'sandbox-app',
  moduleId: __moduleName,
  templateUrl: 'app.html'
})
export class App {
  constructor() {
    this.name = 'Sandbox'
  }
}
