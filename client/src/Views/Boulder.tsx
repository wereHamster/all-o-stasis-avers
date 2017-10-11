import * as Avers from 'avers'
import DatePicker from 'react-datepicker'
import * as moment from 'moment'
import * as React from 'react'
import styled from 'styled-components'

import {role} from '../actions'
import {App, refresh} from '../app'
import {Boulder, grades, sectors} from '../storage'

import {DropDownInput} from './Components/DropdownInput'
import {NumberInput} from './Components/NumberInput'
import {Site} from './Components/Site'
import {BoulderDetails} from './Components/BoulderDetails'

function
boulderDetailsEditor(boulderE: Avers.Editable<Boulder>, app: App): JSX.Element {
    const boulder = boulderE.content

    function onClick(e) {
       e.stopPropagation()
    }

    function changeName(e: React.FormEvent<any>) {
        const value = (e.target as HTMLInputElement).value
        boulder.name = value
    }

    function changeSetDate(date) {
        boulder.setDate = date.valueOf()
        Avers.resetObjectCollection(app.data.activeBouldersCollection)
        refresh(app)
    }

    function getSetDate(): moment.Moment {
        const initialDate = moment.unix(boulder.setDate / 1000.)
        return initialDate
    }

    function setRemoved() {
        const now = Date.now()
        boulder.removed = now.valueOf()
        Avers.resetObjectCollection(app.data.activeBouldersCollection)
        refresh(app)
    }

    function renderRemoved(): JSX.Element {
        if (boulder.removed > 0) {
            return (<p>{moment.unix(boulder.removed / 1000.).format('DD/MM/YYYY')}</p>)
        } else {
            return (<div className='button' onClick={setRemoved}>remove</div>)
        }
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
          <div className='form-row'>
            <div className='label'>Sector</div>
            <div className='content'>
              <DropDownInput object={boulder} field='sector' options={sectors()}></DropDownInput>
            </div>
          </div>
          <div className='form-row'>
            <div className='label'>Grade</div>
            <div className='content'>
              <DropDownInput object={boulder} field='grade' options={grades()}></DropDownInput>
            </div>
          </div>
          <div className='form-row'>
            <div className='label'>Grade Nr</div>
            <div className='content'>
              <NumberInput object={boulder} field='gradeNr'></NumberInput>
            </div>
          </div>
          <div className='form-row'>
            <div className='label'>Set Date</div>
            <div className='content'>
              <DatePicker selected={getSetDate()} onChange={changeSetDate}/>
            </div>
          </div>
            <div className='form-row'>
              <div className='label'>Remove(d)</div>
              <div className='content'>{renderRemoved()}</div>
          </div>
        </div>
      </div>
    )
}

export const boulderView = (boulderId: string) => (app: App) => {

    const boulderC = Avers.lookupEditable<Boulder>(
        app.data.aversH, boulderId)
    const boulderE = boulderC.get(undefined)

    // for now we just render the boulder differently for users
    let boulderRep: JSX.Element
    if (role(app) === 'user') {
        boulderRep = <BoulderDetails boulder={boulderE.content} />
    } else {
        boulderRep = boulderDetailsEditor(boulderE, app)
    }

    return (
      <Site app={app}>
        <div className='boulder'>
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
