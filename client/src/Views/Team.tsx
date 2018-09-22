import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {App} from '../app'
import {Account} from '../storage'

import {useTypeface, heading28} from '../Materials/Typefaces'

import {Site} from './Components/Site'
import {SetterCard} from './Components/SetterCard'

export function
teamView(app: App) {
    const setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
        const accountC = Avers.lookupContent<Account>(app.data.aversH, accountId)
        return accountC.fmap(account =>
            <SetterCard key={accountId} app={app} accountId={accountId} account={account} />,
        ).get(<SetterCard key={accountId} app={app} accountId={accountId} account={undefined} />)
    })

    return (
      <Site app={app}>
        <Root>
          <Logo>Team</Logo>
          <Members>{setters}</Members>
        </Root>
      </Site>
    )
}

const Root = styled.div`
    display: flex;
    flex-direction: column;
    align-items: center;
`

const Logo = styled.div`
    ${useTypeface(heading28)}
    text-align: center;
    margin-top: 4rem;
`
const Members = styled.div`
    padding-top: 80px;
    display: flex;
    flex-direction: row;
    align-items: center;
`
