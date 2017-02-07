/*
module Boulder
( createBoulder
, boulderView
) where
*/


import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../app';

import {Site} from './Components/Site';

import {DropDownInput} from './Components/DropdownInput';
import {NumberInput} from './Components/NumberInput';

import {Boulder} from '../storage';

import {createBoulder} from '../actions';


export function
createBoulderItem(app: App) {

    function createNewBoulder(e) {
        e.stopPropagation();
        createBoulder(app);
    }

    if(app.data.role == "user") {
        return (
            <div className='plain-navbar-item '></div>
        );
    }

    if (app.createBoulderPromise) {
        return (
            <div className='plain-navbar-item '>
                <i className='plus square outlined icon'></i>
                'Creating boulder...'
            </div>
        );

    } else {
        return (
            <div className='navbar-item' onClick={createNewBoulder}>
                <i className='plus square outlined icon'></i>
                'Create boulder'
            </div>
        );
    }
}

function boulderHeader(boulder : Avers.Editable<Boulder>) : any {
    return (
      <div>
        <div className="boulder-header">
          <div className="login">
            <div className="logo">
              {boulder.objectId.substr(0, 10)}
            </div>
            <p className="about">
              "Customize the boulder."
            </p>
          </div>
        </div>
      </div>
    );
}

function boulderDetailsEditor(boulderE: Avers.Editable<Boulder>) {
     var boulder = boulderE.content;

     // FIXME: refactor
     var grades  = ['yellow', 'green', 'orange', 'blue', 'red', 'white'];
     var sectors = ['starship', 'bigboss', 'dune', 'klagemauer', 'kurswand',
         'spektrumone', 'spektrumtwo', 'spektrumthree', 'spektrumfour'];

     function onClick(e) {
        e.stopPropagation();
     }

    function changeName(e: __React.FormEvent) {
        let value = (e.target as HTMLInputElement).value;
        boulder.name = value;
    }

    return (
      <div className='boulder-details-editor'>
        <div className='form'>
          <div className='form-row'>
            <div className='label'>Name</div>
            <div className='content'>
              <input type='text' value={boulder.name} onChange={changeName}
                     onClick={onClick}></input>
            </div>
          </div>
          <div className="form-row">
            <div className="label">Sector</div>
            <div className="content">
              <DropDownInput object={boulder} field='sector' options={sectors}></DropDownInput>
            </div>
          </div>
          <div className="form-row">
            <div className="label">Grade</div>
            <div className="content">
              <DropDownInput object={boulder} field='grade' options={grades}></DropDownInput>
            </div>
          </div>
          <div className="form-row">
            <div className="label">Grade Nr</div>
            <div className="content">
              <NumberInput object={boulder} field='gradeNr'></NumberInput>
            </div>
          </div>
        </div>
      </div>
    );
}


export function
boulderView(app: App, boulderId: string) {

    var boulderC = Avers.lookupEditable<Boulder>(
        app.data.aversH, boulderId);
    var boulderE = boulderC.get(undefined);

    return (
      <Site app={app}>
        <div className="boulder">
          {boulderHeader(boulderE)}
          {boulderDetailsEditor(boulderE)}
        </div>
      </Site>
    );
}

