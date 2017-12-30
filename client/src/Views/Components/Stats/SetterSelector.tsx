import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {Account, BoulderStat, boulderStats} from '../../../storage'
import {App} from '../../../app'
import {accountGravatarUrl} from '../../Account'

import {text, darkGrey, lightGrey} from '../../../Materials/Colors'
import {yellow100, green100, orange100, blue100, red100} from '../../../Materials/Colors'
import {useTypeface, heading18, copy16, copy16Bold, copy14} from '../../../Materials/Typefaces'

import {Section, SectionLink} from './Internal'


export interface SetterSelectorProps {
    app: App
    selectedSetters: string[]

    clear: undefined | (() => void)
    toggle(setterId: string): void
}

export const SetterSelector = ({app, selectedSetters, clear, toggle}: SetterSelectorProps) => {
    const setters = app.data.accountsCollection.ids.get([]).map(accountId => {
        return Avers.lookupContent<Account>(app.data.aversH, accountId).fmap(account => (
            <Setter
                key={accountId}
                accountId={accountId}
                account={account}
                toggle={toggle}
                isSelected={selectedSetters.length === 0 || selectedSetters.indexOf(accountId) !== -1}
            />
        ))
        .get(<Setter key={accountId} accountId={accountId} account={undefined} toggle={toggle} isSelected={false} />)
    })

    return (
        <div style={{marginTop: 80}}>
            <Section>
                Setter
                <SectionLink onClick={clear}>(reset)</SectionLink>
            </Section>

            <div>
                {setters}
            </div>
        </div>
    )
}

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='
const Setter = ({accountId, account, toggle, isSelected}) => (
    <SetterC isSelected={isSelected} onClick={() => toggle(accountId)}>
        <SetterImage isSelected={isSelected} src={account ? accountGravatarUrl(account.email) : placeholderImageSrc} />
        <SetterName isSelected={isSelected}>{(account && account.name !== '') ? account.name : accountId.slice(0, 7)}</SetterName>
    </SetterC>
)

const SetterC = styled.div`
display: flex;
align-items: center;
margin: 4px 0;
padding: 0 0 0 2px;
border-left: 4px solid transparent;
cursor: pointer;
user-select: none;

transition all .2s;

&:hover {
    border-left-color: ${text};
}
`

const SetterImage = styled.img`
display: block;
width: 24px;
height: 24px;
margin-right: 8px;
border-radius: 2px;

transition all .2s;
opacity: ${({isSelected}) => isSelected ? 1 : 0.2}
`

const SetterName = styled.div`
${useTypeface(copy14)}
transition all .2s;
color: ${({isSelected}) => isSelected ? text : lightGrey}
`
