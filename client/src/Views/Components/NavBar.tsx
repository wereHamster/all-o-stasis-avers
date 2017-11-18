import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {useTypeface, copy14} from '../../Materials/Typefaces'
import {lightGrey, text} from '../../Materials/Colors'

import {createBoulder, role} from '../../actions'
import {App, navigateTo, navigateToFn} from '../../app'

import {gradeBackgroundColor, gradeBorderColor, gradeColor} from '../../Materials/Colors'

import logo from '!!url-loader!../../../assets/logo.svg'

export const NavBar = ({app}: {app: App}) => (
    <Root>
        <Logo href='/'>
          <LogoImage src={logo} />
        </Logo>

        <HomeItem />
        <TeamItem />
        {role(app) === 'admin' && <SectorItem />}

        <FlexItem />

        {role(app) !== 'user' && <CreateBoulder app={app} />}
        {app.data.session.objId && <ChangeSecret app={app} />}
        {app.data.session.objId ? <AccountSettings app={app} /> : <SignInItem/>}
    </Root>
)


// ----------------------------------------------------------------------------

const HomeItem = () => (
    <Item isActive={window.location.pathname === '/'} onClick={navigateToFn('/')}>
        Boulders
    </Item>
)

const TeamItem = () => (
    <Item isActive={window.location.pathname === '/team'} onClick={navigateToFn('/team')}>
        Setters
   </Item>
)

const SectorItem = () => (
    <Item isActive={window.location.pathname === '/sector'} onClick={navigateToFn('/sector')}>
        Sectors
   </Item>
)

const StatsItem = () => (
    <Item onClick={navigateToFn('/stats')}>
        <i className='line chart icon'></i>
    </Item>
)

const SignInItem = () => (
    <Item onClick={navigateToFn('/signup')}>
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
align-items: center;
height: 64px;

${useTypeface(copy14)}
color: ${lightGrey};

text-transform: uppercase;
`

const Logo = styled.a`
display: block;
text-decoration: none;
padding: 0 16px 0 16px;

@media (min-width: 480px) {
    padding: 0 48px 0 24px;
}
`
const LogoImage = styled.img`
display: block;
max-width: 40px;
height: 32px;

@media (min-width: 480px) {
    max-width: initial;
}
`

const FlexItem = styled.div`
flex: 1
`

const Item: any = styled<{isActive: boolean}>(({isActive: _, ...props}) => <div {...props} />)`
display: flex;
align-items: center;
align-self: stretch;
padding-right: 16px;
cursor: pointer;
white-space: nowrap;

color: ${({isActive}) => isActive ? text : 'inherit'};

&:hover {
    color: ${text};
}

@media (min-width: 480px) {
    padding-right: 32px;
}
`
