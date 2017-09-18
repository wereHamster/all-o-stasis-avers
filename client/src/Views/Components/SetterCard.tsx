import * as Avers from 'avers'
import * as React from 'react'

import {accountGravatarUrl} from '../Account'
import {App, navigateToFn} from '../../app'
import {Account} from '../../storage'

export interface SetterCardProps {
    app: App
    accountId: string
    account: void | Account
}

export const SetterCard = ({app, accountId, account}: SetterCardProps) => (
    <div className='setter-card'>
        <div onClick={navigateToFn('/account/' + accountId)}>
            <img className='avatar' src={account ? accountGravatarUrl(account.email) : placeholderImageSrc}/>
            <div>{(account && account.name !== '') ? account.name : accountId.slice(0, 2)}</div>
        </div>
    </div>
)

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='
