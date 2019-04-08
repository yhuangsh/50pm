import React, { useState } from 'react';
import styled from 'styled-components';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { 
  faQuestionCircle, 
  faTimesCircle, 
  faPlusSquare 
} from '@fortawesome/free-regular-svg-icons'

import * as defaults from '../lib/defaults';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;

  @media print {
    display: none;
  }
`;

const Title = styled.h2`

  font-family: 'Merienda', cursive;
  font-weight: 100;

  text-align: center;
`;

const IconSet = styled.div`
  
  display: flex;
  flex-flow: row nowrap;
  justify-content: center;
`;

const ActionIcon = styled.div`
  flex: 0 1;
  margin: .2ex .5em;
  color: black;
  
  :hover {
    cursor: pointer;
  }
`;

const InfoBlock = styled.div`
  display: ${props => props.show ? 'block' : 'none'};
  margin-top: 1em;
  padding: .5ex 1em;
  border: 1px solid black;

  font-family: 'Montserrat', sans-serif;
  font-weight: 100;
  text-align: justify;
`;

const DisclaimerBlock = styled.div`
  font-size: 50%;
  text-align: center;
`;

// Components

const QuestionIcon = (props) => {
  return (
    <ActionIcon highlight={props.highlight} onClick={props.onClick}>
      <FontAwesomeIcon icon={faQuestionCircle} />
    </ActionIcon>
  );
}

const PlusOrTimesIcon = (props) => {
  const mode = props.mode;
  const onClick = props.onClick;

  return (
    <ActionIcon onClick={onClick}>
      <FontAwesomeIcon icon={mode === defaults.PM_MODE ? faTimesCircle : faPlusSquare} />
    </ActionIcon>
  )
}

const TopBar = (props) => {
  // Cascafed States

  const mode = props.mode;
  const title = defaults.titles[mode];
  const onClickTimesOrPlusIcon = props.onClickTimesOrPlusIcon;
  
  // Local States
  const [showHelp, setShowHelp] = useState(defaults.showHelp);

  const onClickQuestionIcon = (e) => {
    setShowHelp(!showHelp);
  } 

  return (
    <Frame>
      <Title>{title}</Title>
      <IconSet>
        <PlusOrTimesIcon mode={mode} onClick={onClickTimesOrPlusIcon} />
        <QuestionIcon onClick={onClickQuestionIcon} />
      </IconSet>
      <InfoBlock show={showHelp}>
        <p>
          Fifty Plus Minus generates 50 random simple arithmetic equations with unknowns on either side of the
          equal sign for parents to print on a A4 paper. To print, just use your browser's own print menu. You 
          may need to <strong>turn off page margins</strong> added by your browser to fit everything in.
        </p>
        <br/>
        <DisclaimerBlock>
          <p>
            This websites uses CNZZ web tracking <span id={'cnzz_stat_icon_' + process.env.REACT_APP_CNZZ_ID}/>
            &nbsp; to help understand how you use it so we can improve. 
          </p>
          <p>
            No cookie is used at moment. We plan to add cookie in the next release to improve user experience, 
            such as saving your options for next use.
          </p>
          <p>
            This website is registered and hosted in China. Registration number: 沪ICP备18046429号-1
          </p>
        </DisclaimerBlock>
      </InfoBlock>
    </Frame>
  );
}

export default TopBar;

// Utilities