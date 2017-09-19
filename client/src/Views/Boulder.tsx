/*
module Boulder
( createBoulder
, boulderView
) where
*/

import * as Avers from 'avers';
import DatePicker from 'react-datepicker';
import * as moment from 'moment';
import * as React from 'react';
import styled from 'styled-components'

import {createBoulder, role} from '../actions';
import {App, refresh} from '../app';
import {Boulder, grades, sectors} from '../storage';

import {DropDownInput} from './Components/DropdownInput';
import {NumberInput} from './Components/NumberInput';
import {Site} from './Components/Site';

export function
createBoulderItem(app: App) : JSX.Element {
    function createNewBoulder(e) {
        e.stopPropagation();
        createBoulder(app);
    }

    if(role(app) == "user") {
        return (
          <div className='plain-navbar-item '></div>
        );
    }

    if (app.createBoulderPromise) {
        return (
          <div className='plain-navbar-item '>
            <i className='plus square outlined icon'></i>
            Creating boulder...
          </div>
        );

    } else {
        return (
          <div onClick={createNewBoulder}>
            <i className='plus square outlined icon'></i>
            Boulder
          </div>
        );
    }
}

function
boulderDetails(boulder: Boulder) : JSX.Element {
    var setDate = moment.unix(boulder.setDate / 1000.).toISOString();

    return (
      <div className='details'>
        <ul>
          <li>{boulder.name}</li>
          <li>{boulder.sector}</li>
          <li>{boulder.gradeNr} {boulder.grade}</li>
          <li>{setDate}</li>
        </ul>
      </div>
    );
}

function 
boulderDetailsEditor(boulderE: Avers.Editable<Boulder>, app: App) : JSX.Element {
    var boulder = boulderE.content;

    function onClick(e) {
       e.stopPropagation();
    }

    function changeName(e: React.FormEvent<any>) {
        let value = (e.target as HTMLInputElement).value;
        boulder.name = value;
    }

    function changeSetDate(date) {
        boulder.setDate = date.valueOf();
        Avers.resetObjectCollection(app.data.activeBouldersCollection);
        refresh(app);
    }

    function getSetDate() : moment.Moment {
        const initialDate = moment.unix(boulder.setDate / 1000.);
        return initialDate;
    }

    function setRemoved() {
        const now = Date.now();
        boulder.removed = now.valueOf();
        Avers.resetObjectCollection(app.data.activeBouldersCollection);
        refresh(app);
    }

    function renderRemoved() : JSX.Element {
        if (boulder.removed >= 0)
            return (<p>{moment.unix(boulder.removed / 1000.).format('DD/MM/YYYY')}</p>);
        else 
            return (<div className="button" onClick={setRemoved}>remove</div>);
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
              <DropDownInput object={boulder} field='sector' options={sectors()}></DropDownInput>
            </div>
          </div>
          <div className="form-row">
            <div className="label">Grade</div>
            <div className="content">
              <DropDownInput object={boulder} field='grade' options={grades()}></DropDownInput>
            </div>
          </div>
          <div className="form-row">
            <div className="label">Grade Nr</div>
            <div className="content">
              <NumberInput object={boulder} field='gradeNr'></NumberInput>
            </div>
          </div>
          <div className="form-row">
            <div className="label">Set Date</div>
            <div className="content">
              <DatePicker selected={getSetDate()} onChange={changeSetDate}/>
            </div>
          </div>
            <div className="form-row">
              <div className="label">Remove(d)</div>
              <div className="content">{renderRemoved()}</div>
          </div> 
        </div>
      </div>
    );
}

export const boulderView = (boulderId: string) => (app: App) => {

    var boulderC = Avers.lookupEditable<Boulder>(
        app.data.aversH, boulderId);
    var boulderE = boulderC.get(undefined);

    // for now we just render the boulder differently for users
    var boulderRep : JSX.Element;
    if (role(app) == "user") {
        boulderRep = boulderDetails(boulderE.content);
    } else {
        boulderRep = boulderDetailsEditor(boulderE, app);
    }

    return (
      <Site app={app}>
        <div className="boulder">
          <BoulderHeader>
            {boulderE.content.name || boulderE.objectId.substr(0, 10)}
          </BoulderHeader>
          {boulderRep}
        </div>
      </Site>
    )
}


// ----------------------------------------------------------------------------

const BoulderHeader = styled.div`
    text-align: center;
    font-size: 4rem;
    font-family: "trajan-sans-pro";
    margin: 4rem 0 2rem;
`
