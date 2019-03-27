import React from 'react';
import styled from 'styled-components';

import TopBar from '../components/TopBar';
import SettingsPanel from '../components/SettingsPanel';

// Constants

// Stylings

const Frame = styled.div`
  width: 100%;
`;

// Components

const AppView = () => {
  return (
    <Frame>
      <TopBar />
      <SettingsPanel />
    </Frame>
  );
}

export default AppView;

// Utilities