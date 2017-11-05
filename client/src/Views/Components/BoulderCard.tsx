import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {accountGravatarUrl} from '../Account'
import {App, navigateTo} from '../../app'
import {Account, Boulder, prettyPrintSector} from '../../storage'

import {lightGrey, darkGrey, text, gradeBackgroundColor, gradeBorderColor, gradeColor} from '../../Materials/Colors'
import {useTypeface, copy16} from '../../Materials/Typefaces'

import {BoulderId} from './BoulderId'

export interface BoulderCardProps {
    app: App
    boulderE: Avers.Editable<Boulder>
}

export class BoulderCard extends React.Component<BoulderCardProps> {
    onClick = () => {
        navigateTo('/boulder/' + this.props.boulderE.objectId)
    }

    render() {
        const {app, boulderE} = this.props
        const {objectId, content} = boulderE
        const {grade, gradeNr, sector, setter} = content

        return (
            <Card onClick={this.onClick} grade={grade}>
                <BoulderId grade={grade}>{gradeNr}</BoulderId>
                <Meta>
                    <Sector>{prettyPrintSector(sector)}</Sector>
                    <Setters>
                        {setter.map((setterId, index) => <BoulderCardSetter key={index} app={app} setterId={setterId} />)}
                    </Setters>
                </Meta>
            </Card>
        )
    }
}

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='
const BoulderCardSetter = ({app, setterId}: {app: App, setterId: string}) => (
    <Setter src={Avers.lookupContent<Account>(app.data.aversH, setterId).fmap(account => accountGravatarUrl(account.email)).get(placeholderImageSrc)} />
)



// ----------------------------------------------------------------------------

const Card: any = styled.div`
    display: flex;
    flex-direction: row;
    cursor: pointer;
    padding: 8px 16px;
    width: 100%;
    overflow: hidden;

    & ${BoulderId} {
        transition: transform .2s;
    }

    &:hover ${BoulderId} {
        box-shadow: 0 0 8px rgba(99, 85, 25, .5);
        transform: scale(1.08);
    }

    @media (min-width: 480px) {
        padding: 16px 24px;
        width: calc(100% / 2);
    }

    @media (min-width: 720px) {
        padding: 16px 24px;
        width: calc(100% / 3);
    }

    @media (min-width: 960px) {
        width: calc(100% / 4);
    }

    @media (min-width: 1200px) {
        width: calc(100% / 5);
    }
`

const Meta = styled.div`
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    align-items: center;
    flex: 1;
    margin-left: 12px;

    @media (min-width: 480px) {
        flex-direction: column;
        align-items: flex-start;
        justify-content: center;
    }
`

const Sector = styled.div`
    ${useTypeface(copy16)}
    white-space: nowrap;
    line-height: 1;
    text-transform: uppercase;
`

const Setters = styled.div`
    display: flex;
    flex-direction: row;

    @media (min-width: 480px) {
        margin-top: 8px;
    }
`

const Setter = styled.img`
    width: 32px;
    height: 32px;
    margin: 0 0 0 4px;
    display: block;
    border-radius: 2px;

    @media (min-width: 480px) {
        width: 24px;
        height: 24px;
        margin: 0 4px 0 0;
    }
`
