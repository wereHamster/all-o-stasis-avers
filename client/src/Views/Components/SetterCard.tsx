import * as React from 'react'
import styled from 'styled-components'

import {accountGravatarUrl} from '../Account'
import {App, navigateToFn} from '../../app'
import {Account} from '../../storage'

export interface SetterCardProps {
    app: App
    accountId: string
    account: void | Account
}

export const SetterCard = ({accountId, account}: SetterCardProps) => (
    <Setter>
        <div onClick={navigateToFn('/account/' + accountId)}>
            <img src={account ? accountGravatarUrl(account.email) : placeholderImageSrc}/>
            <div>{(account && account.name !== '') ? account.name : accountId.slice(0, 2)}</div>
        </div>
    </Setter>
)

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='

const Setter = styled.div`
    display: flex;
    flex-direction: row;
    align-items: center;
    text-align: center;
    margin-left: 10px;
    margin-right: 10px;

    img {
        height: 50px;
        border: 1px solid #999;
        border-radius: 50%;
    }
`
