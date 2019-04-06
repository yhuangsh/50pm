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
  let [digitOpt, setDigitOpt] = useState(defaults.digits);
  let [unknownOpt, setUnknownOpt] = useState(defaults.unknowns);
  let [pageOpt, setPageOpt] = useState(defaults.pages);

  let [equs, setEqus] = useState(genEqusList(pages(pageOpt), 50, digitOpt, unknownOpt));

  const onClickDigit = (e, newDigitOpt) => {
    setDigitOpt(newDigitOpt);
    setEqus(genEqusList(pages(pageOpt), 50, newDigitOpt, unknownOpt));
    //console.log('AppView.onClickDigit: newDigitOpt = ', newDigitOpt);
  }

  const onClickUnknown = (e, newUnknownOpt) => {
    setUnknownOpt(newUnknownOpt);
    setEqus(genEqusList(pages(pageOpt), 50, digitOpt, newUnknownOpt));
    //console.log('AppView.onClickUnknown: newUnknownOpt =', newUnknownOpt);
  }

  const onClickPage = (e, newPageOpt) => {
    setPageOpt(newPageOpt);
    setEqus(genEqusList(pages(newPageOpt), 50, digitOpt, unknownOpt));
    //console.log('AppView.onClickPage: newPageOpt =', newPageOpt);
  }

  return (
    <Frame>
      <TopBar />
      <SettingsPanel 
        digitOpt={digitOpt} onClickDigit={onClickDigit}
        unknownOpt={unknownOpt} onClickUnknown={onClickUnknown}
        pageOpt={pageOpt} onClickPage={onClickPage}
      />
      <PrintPreview equs={equs} />
    </Frame>
  );
}

export default AppView;

// Utilities

const pages = (pageOpt) => {
  let [p1, p2, p3, p4] = pageOpt;

  if (p1) return 1;
  if (p2) return 2;
  if (p3) return 4;
  if (p4) return 10;

  console.log('ERROR: bad pageOpt');
  return 0;
}