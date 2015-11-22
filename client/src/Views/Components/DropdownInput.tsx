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

    onChange = (e: __React.FormEvent) => {
        let value = (e.target as HTMLSelectElement).value;

        this.setState({ selectedValue: value });
        if (isValidNumber(value)) {
            this.props.object[this.props.field] = parseFloat(value);
        }
    };

    render() {
        let className = '';
        if (!isValidNumber(this.state.selectedValue)) {
            className = 'invalid';
        }

        function onClick(e) {
            e.stopPropagation();
        }

        var options = this.props.options.map( entry => {
            return (
              <option value={entry}>
                entry
              </option>
            );
        });

        return (
            <select name={this.props.field} className={className} onClick={onClick} onChange={this.onChange}>
              options
            </select>);
    }

    componentWillReceiveProps(props) {
        if (ReactDOM.findDOMNode(this) !== document.activeElement) {
            this.setState(this.initialState(props));
        }
    }
}

export var DropDownInput = React.createFactory(DropdownInputSpec);
