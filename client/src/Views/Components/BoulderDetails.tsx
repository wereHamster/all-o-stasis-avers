import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {App} from '../../app'
import {Account, Boulder} from '../../storage'
import {accountGravatarUrl} from '../Account'

import {text} from '../../Materials/Colors'
import {useTypeface, copy16, copy16Bold} from '../../Materials/Typefaces'

import {SectorPicker} from './SectorPicker'

export const BoulderDetails = ({app, boulder}: {app: App, boulder: Boulder}) => (
    <Root>
        <Section>Sector</Section>
        <SectorPicker sectors={[boulder.sector]} onChange={() => {}} />

        <Section>Setters</Section>
        <div style={{display: 'flex'}}>
        {boulder.setter.map((setterId, index) => (
            <Setter key={index} app={app} setterId={setterId} />
        ))}
      </div>
    </Root>
)


// ----------------------------------------------------------------------------

const Root = styled.div`
max-width: 400px;
`

const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 40px 0 12px;
&:first-of-type {
    padding: 0 0 12px;
}
`


// ----------------------------------------------------------------------------

const Setter = ({app, setterId}) => {
    return Avers.lookupContent<Account>(app.data.aversH, setterId).fmap(account => (
        <SetterContainer>
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
