import * as Avers from 'avers'
import DatePicker from 'react-datepicker'
import * as moment from 'moment'
import * as React from 'react'
import styled from 'styled-components'

import {role} from '../actions'
import {App, refresh} from '../app'
import {Boulder, grades, sectors} from '../storage'

import {text, darkGrey} from '../Materials/Colors'
import {useTypeface, copy16, copy16Bold, copy14} from '../Materials/Typefaces'

import {DropDownInput} from './Components/DropdownInput'
import {NumberInput} from './Components/NumberInput'
import {Site} from './Components/Site'
import {BoulderDetails} from './Components/BoulderDetails'
import {BoulderId} from './Components/BoulderId'
import {SectorPicker} from './Components/SectorPicker'

function
BoulderDetailsEditor({app, boulderE}: {app: App, boulderE: Avers.Editable<Boulder>}) {
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

    function changeRemovedDate(date) {
        boulder.removed = date.valueOf()
        Avers.resetObjectCollection(app.data.activeBouldersCollection)
        refresh(app)
    }

    function setRemoved() {
        changeRemovedDate(Date.now())
    }

    function clearRemoved() {
      changeRemovedDate(0)
    }

    return (
      <div>
        <Section>Sector</Section>
        <div>
          <div style={{maxWidth: 400}}>
            <SectorPicker sector={boulder.sector} onChange={sector => { boulder.sector = sector }}/>
          </div>
        </div>

        <Section>Grade</Section>
        <div>
          <div style={{display: 'flex', justifyContent: 'space-between'}}>
            <GradeSelect boulder={boulder} grade='yellow' />
            <GradeSelect boulder={boulder} grade='green' />
            <GradeSelect boulder={boulder} grade='orange' />
            <GradeSelect boulder={boulder} grade='blue' />
            <GradeSelect boulder={boulder} grade='red' />
            <GradeSelect boulder={boulder} grade='white' />
          </div>
          <div style={{marginTop: 12}}>
            <NumberInput object={boulder} field='gradeNr'></NumberInput>
          </div>
        </div>

        <Section>Set Date</Section>
        <DatePicker selected={getSetDate()} onChange={changeSetDate} />

        <Section>Danger Zone</Section>
        {(() => {
          if (boulder.removed > 0) {
            return (
              <div>
                <SectionLabel>The boulder was removed on</SectionLabel>
                <div style={{display: 'flex'}}>
                  <DatePicker
                    selected={moment.unix(boulder.removed / 1000.)}
                    onChange={changeRemovedDate}
                  />
                  <DangerButton onClick={clearRemoved}>Put back on wall</DangerButton>
                </div>
              </div>
            )
          } else {
            return (
              <div>
                <DangerButton onClick={setRemoved}>
                  Remove
                </DangerButton>
              </div>
            )
          }
        })()}
      </div>
    )
}

export const boulderView = (boulderId: string) => (app: App) => {
    return Avers.lookupEditable<Boulder>(app.data.aversH, boulderId).fmap(boulderE => {
      const boulderRep = role(app) === 'user'
        ? <BoulderDetails boulder={boulderE.content} />
        : <BoulderDetailsEditor app={app} boulderE={boulderE} />

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
    }).get(<Site app={app}>Loading…</Site>)
}


// ----------------------------------------------------------------------------

const GradeSelect = ({boulder, grade}: any) => (
    <GradeSelectButton onClick={() => { boulder.grade = grade }}>
        <BoulderId grade={grade}>
          {boulder.grade === grade ? <Cross /> : ''}
        </BoulderId>
    </GradeSelectButton>
)

const GradeSelectButton = styled.div`
cursor: pointer;

& ${BoulderId} {
  transition: transform .2s;
}

&:hover ${BoulderId} {
  transform: scale(1.08);
}
`

const Cross = () => (
    <svg width='24' height='24'>
      <path stroke='currentColor' strokeWidth='2' d='M 2 2 L 22 22' />
      <path stroke='currentColor' strokeWidth='2' d='M 22 2 L 2 22' />
    </svg>
)


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

const SectionLabel = styled.div`
${useTypeface(copy14)}
color: ${darkGrey};

padding: 0 0 4px;
`

const DangerButton = styled.button`
${useTypeface(copy16)}
font-size: 1rem;
line-height: normal;
margin: 0;
padding: .3rem .8rem;
display: inline-block;
background-color: #dfe9f1;
text-align: center;
color: red;
outline: none;
border: 1px solid #92a0ad;
border-radius: 2px;
cursor: pointer;

transition: all .2s;

&:hover {
  background-color: #b2d0e6;
  color: #a00c0c;
}
`
