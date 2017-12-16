import * as Avers from 'avers'
import * as React from 'react'

import {App} from '../../app'

import {NavBar} from './NavBar'
import {TransientNotification} from './TransientNotification'

export const Site = ({app, children}: {app: App, children?: JSX.Element | JSX.Element[]}) => {
    function onClick() {
        Avers.startNextGeneration(app.data.aversH)
    }

    return (
        <div style={{minHeight: '100vh', display: 'flex', flexDirection: 'column'}} onClick={onClick}>
            <NavBar app={app} />
            {children}
            <TransientNotification app={app} />
        </div>
    )
}
