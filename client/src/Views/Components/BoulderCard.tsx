import * as Avers from 'avers';
import * as React from 'react';

import {accountGravatarUrl} from '../Account';
import {App, navigateTo} from '../../app';
import {Account, Boulder, prettyPrintSector} from '../../storage';

export interface BoulderCardProps {
    app: App;
    boulderE: Avers.Editable<Boulder>;
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
            <div className="boulder-card" onClick={this.onClick}>
                <div className={`boulder-card-id ${grade}`}>
                    {gradeNr}
                </div>
                <div className="boulder-card-meta">
                    <div className="boulder-card-sector">{prettyPrintSector(sector)}</div>
                    <div className="boulder-card-setters">
                        {setter.map(setterId => <BoulderCardSetter app={app} setterId={setterId} />)}
                    </div>
                </div>
            </div>
        );
    }
}


const placeholderImageSrc = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII="
const BoulderCardSetter = ({app, setterId}: {app: App, setterId: string}) => (
    <img
        className="boulder-card-setter"
        src={Avers.lookupContent<Account>(app.data.aversH, setterId).fmap(account => accountGravatarUrl(account.email)).get(placeholderImageSrc)}
    />
)
