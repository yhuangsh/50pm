import React, { useState } from 'react';
import styled from 'styled-components';

import TopBar from '../components/TopBar';
import SettingsPanel from '../components/SettingsPanel';
import PrintPreview from '../components/PrintPreview';

import * as defaults from '../lib/defaults';
import { genEqusList } from '../lib/equ';

// Constants

// Stylings

const Frame = styled.div`
  width: 100%;
`;

// Components

const AppView = () => {
  //const [mode, setMode] = useState(defaults.PM_MODE);

  const [pmtdOpt, setPmtdOpt] = useState(defaults.pmtd);
  const [digitOpt, setDigitOpt] = useState(defaults.digits);
  const [unknownOpt, setUnknownOpt] = useState(defaults.unknowns);
  const [pageOpt, setPageOpt] = useState(defaults.pages);

  const [equs, setEqus] = useState(genEqusList(mode(pmtdOpt), pages(pageOpt), 50, digitOpt, unknownOpt));

  const onClickPmtd = (e, newPmtdOpt) => {
    setPmtdOpt(newPmtdOpt);
    setEqus(genEqusList(mode(newPmtdOpt), pages(pageOpt), 50, digitOpt, unknownOpt));
    //console.log('AppView.onClickPmtd: newPmtdOpt = ', newDPmtdpt);
  }

  const onClickDigit = (e, newDigitOpt) => {
    setDigitOpt(newDigitOpt);
    setEqus(genEqusList(mode, pages(pageOpt), 50, newDigitOpt, unknownOpt));
    //console.log('AppView.onClickDigit: newDigitOpt = ', newDigitOpt);
  }

  const onClickUnknown = (e, newUnknownOpt) => {
    setUnknownOpt(newUnknownOpt);
    setEqus(genEqusList(mode, pages(pageOpt), 50, digitOpt, newUnknownOpt));
    //console.log('AppView.onClickUnknown: newUnknownOpt =', newUnknownOpt);
  }

  const onClickPage = (e, newPageOpt) => {
    setPageOpt(newPageOpt);
    setEqus(genEqusList(mode, pages(newPageOpt), 50, digitOpt, unknownOpt));
    //console.log('AppView.onClickPage: newPageOpt =', newPageOpt);
  }

  return (
    <Frame>
      <TopBar />
      <SettingsPanel 
        pmtdOpt={pmtdOpt} onClickPmtd={onClickPmtd}
        digitOpt={digitOpt} onClickDigit={onClickDigit}
        digitOptForTimes={digitOpt} onClickDigitForTimes={onClickDigit}
        unknownOpt={unknownOpt} onClickUnknown={onClickUnknown}
        pageOpt={pageOpt} onClickPage={onClickPage}
      />
      <PrintPreview equs={equs} />
    </Frame>
  );
}

export default AppView;

// Utilities

const mode = (pmtdOpt) => {
  const [p1, p2] = pmtdOpt;

  if (p1) return defaults.PM_MODE;
  if (p2) return defaults.TD_MODE;

  console.log('ERROR: bad pmtdOpt');
  return 0;
}

const pages = (pageOpt) => {
  const [p1, p2, p3, p4] = pageOpt;

  if (p1) return 1;
  if (p2) return 2;
  if (p3) return 4;
  if (p4) return 10;

  console.log('ERROR: bad pageOpt');
  return 0;
}