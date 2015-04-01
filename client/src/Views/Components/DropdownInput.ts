/*
module DropDownInput
( DropDownInput
) where
*/


export interface DropdownInputProps {
    object  : any;
    field   : string;
    options : string[];
}

interface DropdownInputState {
    selectedValue : string;
}

// The value is only valid if it can be fully parsed into a number.
function isValidNumber(value: string): boolean {
    return true;
}

function asString(value) {
    if (value === null || value === undefined) {
        return '';
    } else {
        return value;
    }
}


class DropdownInputSpec extends React.Component<DropdownInputProps, DropdownInputState> {

    initialState(props) {
        var selectedValue = props.object[props.field];
        return { selectedValue : asString(selectedValue) };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    render() {
        var valueLink =
            { value: this.state.selectedValue
            , requestChange: value => {
                this.setState({ selectedValue: value });
                this.props.object[this.props.field] = value;
              }
            };

        //FIXME:
        var className = React.addons.classSet(
            { invalid : !isValidNumber(this.state.selectedValue)
            }
        );

        function onClick(e) {
            e.stopPropagation();
        }

        var options = this.props.options.map( entry => {
            return React.DOM.option
                ( { value : entry }
                , entry
                );
        });

        return React.DOM.select
            ( { name : this.props.field, className: className, valueLink: valueLink, onClick: onClick }
            , options
            );
    }

    componentWillReceiveProps(props) {
        if (React.findDOMNode(this) !== document.activeElement) {
            this.setState(this.initialState(props));
        }
    }
}

export var DropDownInput = React.createFactory(DropdownInputSpec);
