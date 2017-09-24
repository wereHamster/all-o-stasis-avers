import * as Avers from 'avers'
import DatePicker from 'react-datepicker'
import * as moment from 'moment'
import * as React from 'react'
import styled from 'styled-components'

import {role} from '../actions'
import {App, refresh} from '../app'
import {Boulder, grades, sectors} from '../storage'

import {text} from '../Materials/Colors'
import {useTypeface, copy16Bold} from '../Materials/Typefaces'

import {DropDownInput} from './Components/DropdownInput'
import {NumberInput} from './Components/NumberInput'
import {Site} from './Components/Site'
import {BoulderDetails} from './Components/BoulderDetails'
import {BoulderId} from './Components/BoulderId'

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
      <div>
        <Section>Sector</Section>
        <div>
          <DropDownInput object={boulder} field='sector' options={sectors()}></DropDownInput>
        </div>

        <Section>Grade</Section>
        <div>
          <div>
            <DropDownInput object={boulder} field='grade' options={grades()}></DropDownInput>
          </div>
          <div>
            <NumberInput object={boulder} field='gradeNr'></NumberInput>
          </div>
        </div>

        <Section>Set Date</Section>
        <div>
          <DatePicker selected={getSetDate()} onChange={changeSetDate}/>
        </div>

        <Section>Remove(d)</Section>
        <div>
          {renderRemoved()}
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
          <div style={{display: 'flex', padding: 24}}>
            <BoulderId grade={boulderE.content.grade}>{boulderE.content.gradeNr}</BoulderId>
            <div style={{paddingLeft: 24}}>
              {boulderRep}
            </div>
          </div>
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

const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 80px 0 12px;
&:first-of-type {
    padding: 0 0 12px;
}
`
