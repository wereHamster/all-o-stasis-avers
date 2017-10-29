import * as Avers from 'avers'
import DatePicker from 'react-datepicker'
import * as moment from 'moment'
import * as React from 'react'
import styled from 'styled-components'

import {role} from '../actions'
import {App, navigateToFn, refresh} from '../app'
import {Account, Boulder, grades, sectors} from '../storage'
import {accountGravatarUrl} from './Account'

import {text, darkGrey, lightGrey} from '../Materials/Colors'
import {useTypeface, heading18, copy16, copy16Bold, copy14} from '../Materials/Typefaces'

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

    function addSetter(accountId) {
      boulder.setter = [...boulder.setter, accountId]
    }

    function removeSetter(accountId) {
      boulder.setter = boulder.setter.filter(x => x !== accountId)
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

        <Section>Setters</Section>
        <div style={{display: 'flex'}}>
          {boulder.setter.map(setterId => (
            <Setter key={setterId} app={app} setterId={setterId} onClick={removeSetter} />
          ))}
          <AddSetter app={app} addSetter={addSetter} />
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
        ? <BoulderDetails app={app} boulder={boulderE.content} />
        : <BoulderDetailsEditor app={app} boulderE={boulderE} />

      return (
        <Site app={app}>
          <div className='boulder'>
            <div style={{padding: '40px 20px 20px'}}>
              <Back onClick={navigateToFn('/')} style={{display: 'flex', alignItems: 'center'}}>
                <BackIcon viewBox='0 0 50 40' height='14'>
                  <path d='M20 4L4 20L20 36M4 20L46 20' fill='transparent' stroke='currentColor' strokeWidth='5' strokeLinecap='round' strokeLinejoin='round' />
                </BackIcon>

                <BackText>
                  Back to all boulders
                </BackText>
              </Back>
            </div>
            <div style={{display: 'flex', padding: 24}}>
              <BoulderId grade={boulderE.content.grade}>{boulderE.content.gradeNr}</BoulderId>
              <div style={{paddingLeft: 24}}>
                {boulderRep}
              </div>
            </div>
          </div>
        </Site>
      )
    }).get(<Site app={app}><div>Loading…</div></Site>)
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

const Setter = ({app, setterId, onClick}) => {
  return Avers.lookupContent<Account>(app.data.aversH, setterId).fmap(account => (
    <SetterContainer onClick={() => { onClick(setterId) }}>
      <SetterImage src={accountGravatarUrl(account.email)} />
      <SetterName>{(account.name !== '') ? account.name : setterId.slice(0, 2)}</SetterName>
    </SetterContainer>
  )).get(<div>{setterId}</div>)
}

const SetterContainer = styled.div`
margin-right: 8px;
cursor: pointer;
`

const SetterImage = styled.img`
display: block;
width: 60px;
height: 60px;
`

const SetterName = styled.div`
${useTypeface(copy16)}
color: ${text};
text-align: center;
`


// ----------------------------------------------------------------------------

class AddSetter extends React.Component<any, any> {
  state = {
    isOpen: false,
  }

  open = () => {
    this.setState({isOpen: true})
  }
  close = () => {
    this.setState({isOpen: false})
  }
  addSetter = (accountId) => {
    this.props.addSetter(accountId)
    this.close()
  }

  render() {
    return (
      <AddSetterContainer>
        <svg width='60' height='60' onClick={this.open}>
          <path d='M10 30 h40 M30 10 v40' />
        </svg>
        {this.state.isOpen && (
          <SetterPicker
            app={this.props.app}
            dismiss={this.close}
            addSetter={this.addSetter}
          />
        )}
      </AddSetterContainer>
    )
  }
}

const AddSetterContainer = styled.div`
width: 60px;
height: 60px;
cursor: pointer;

& svg path {
  stroke: ${lightGrey};
  stroke-width: 6;
  stroke-linecap: round;
  transition: fill .12s;
}
&:hover svg path {
  stroke: ${text};
}
`


// ----------------------------------------------------------------------------

class SetterPicker extends React.Component<any> {
  onKeyPress = (ev) => {
    if (ev.key === 'Escape') {
      this.props.dismiss()
    }
  }

  componentDidMount() {
    window.addEventListener('keyup', this.onKeyPress)
  }
  componentWillUnmount() {
    window.removeEventListener('keyup', this.onKeyPress)
  }

  render() {
    const {app, dismiss, addSetter} = this.props
    const setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
      const accountC = Avers.lookupContent<Account>(app.data.aversH, accountId)
      return accountC.fmap(account => (
        <Setter
          key={accountId}
          app={app}
          setterId={accountId}
          onClick={() => { addSetter(accountId) }}
        />
      )).get(<div />)
    })

    return (
      <div style={{position: 'fixed', zIndex: 10, top: 0, right: 0, bottom: 0, left: 0, padding: 60, background: '#f1f1f1'}}>
        <div style={{position: 'fixed', top: 20, right: 20}} onClick={dismiss}>
          <svg width='40' height='40'>
            <path d='M10 10 L30 30 M30 10 L10 30' stroke='black' strokeWidth='7' strokeLinecap='round' />
          </svg>
        </div>

        <div style={{textAlign: 'center', marginBottom: 60, fontSize: 32}}>Pick a setter</div>

        <div style={{display: 'flex', flexWrap: 'wrap'}}>
          {setters}
        </div>
      </div>
    )
  }
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


// ----------------------------------------------------------------------------

const Back = styled.div`
display: flex;
align-items: center;
cursor: pointer;

color: ${darkGrey};
transition: color .12s;

&:hover {
  color: ${text};
}
`

const BackIcon = styled.svg`
display: block;
margin-right: 8px;
`

const BackText = styled.div`
${useTypeface(copy16)}
line-height: 1;
`
