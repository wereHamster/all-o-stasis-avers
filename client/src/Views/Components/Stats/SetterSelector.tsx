import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {Account} from '../../../storage'
import {App} from '../../../app'
import {accountGravatarUrl} from '../../Account'

import {text, lightGrey} from '../../../Materials/Colors'
import {useTypeface, copy14} from '../../../Materials/Typefaces'

import {Section, SectionLink} from './Internal'


export interface SetterSelectorProps {
    app: App
    selectedSetters: string[]

    clear: undefined | (() => void)
    toggle(setterId: string): void
}

export const SetterSelector = ({app, selectedSetters, clear, toggle}: SetterSelectorProps) => {
    const setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
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
        <>
            <Section>
                Setter
                <SectionLink onClick={clear}>(reset)</SectionLink>
            </Section>

            <div>
                {setters}
            </div>
        </>
    )
}

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='
const Setter = ({accountId, account, toggle, isSelected}) => (
    <SetterC onClick={() => toggle(accountId)}>
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

const SetterImage = styled<{ isSelected: boolean }, "img">("img")`
display: block;
width: 24px;
height: 24px;
margin-right: 8px;
border-radius: 2px;

transition all .2s;
opacity: ${({isSelected}) => isSelected ? 1 : 0.2}
`

const SetterName = styled<{ isSelected: boolean }, "div">("div")`
${useTypeface(copy14)}
transition all .2s;
color: ${({isSelected}) => isSelected ? text : lightGrey}
`
