import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {createBoulder, role} from '../../actions'
import {App, navigateTo, navigateToFn} from '../../app'
import * as Boulder from '../Boulder'
import {Account} from '../../storage'

import {gradeBackgroundColor, gradeBorderColor, gradeColor} from '../../Materials/Colors'

export const NavBar = ({app}: {app: App}) => (
    <Root>
        <HomeItem/>
        <TeamItem/>
        <FlexItem/>
        {app.data.session.objId && <CreateBoulder app={app} />}
        {app.data.session.objId && <ChangeSecret app={app} />}
        {app.data.session.objId ? <AccountSettings app={app} /> : <SignInItem/>}
    </Root>
)


// ----------------------------------------------------------------------------

const HomeItem = () => (
    <Item onClick={navigateToFn('/')}>
        <i className='home icon'></i>
    </Item>
)

const TeamItem = () => (
    <Item onClick={navigateToFn('/team')}>
        <i className='users icon'></i>
   </Item>
)

const StatsItem = () => (
    <Item onClick={navigateToFn('/stats')}>
        <i className='line chart icon'></i>
    </Item>
)

const SignInItem = () => (
    <Item onClick={navigateToFn('/login')}>
        <i className='sign in icon'></i>
        Login
    </Item>
)

const CreateBoulder = ({app}: {app: App}) => (
    <Item onClick={createNewBoulder(app)}>
        <i className='plus square outlined icon'></i>
        Boulder
    </Item>
)

const AccountSettings = ({app}: {app: App}) => (
    <Item onClick={navigateToFn('/account/' + app.data.session.objId)}>
        <i className='settings icon'></i>Account Settings
    </Item>
)

const ChangeSecret = ({app}: {app: App}) => (
    <Item onClick={navigateToFn('/account/' + app.data.session.objId + '/updateSecret')}>
        <i className='key icon'></i>Change Secret
    </Item>
)

const SignOut = ({app}: {app: App}) => (
    <Item onClick={doSignOutF(app)}>
        <i className='sign out icon'></i>Logout
    </Item>
)

function doSignOutF(app: App) {
    return e => {
        e.stopPropagation()
        Avers.signout(app.data.session).then(() => {
            navigateTo('/')
        })
    }
}

function createNewBoulder(app: App) {
    return (e) => {
        e.stopPropagation()
        createBoulder(app)
    }
}

const Root = styled.div`
    display: flex;
    flex-direction: row;
    height: 2.5rem;
    font-size: 1.2rem;
    line-height: 2.5rem;
    color: rgba(33, 33, 33, 0.7);
    border-top: 1px solid #ddd;
    border-bottom: 1px solid #ddd;
    margin: 0.5rem;
`

const FlexItem = styled.div`
   flex: 1
`

const Item = styled.div`
    padding: 0 .5rem;

    &:hover {
        cursor: pointer;
        background-color: #999;
    }
`
