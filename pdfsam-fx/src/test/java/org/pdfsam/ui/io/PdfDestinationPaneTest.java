/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.io;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;

import javax.inject.Inject;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.pdfsam.context.UserContext;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class PdfDestinationPaneTest {

    private static final String MODULE = "MODULE";
    @Rule
    public InitializeJavaFxThreadRule fxThread = new InitializeJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Inject
    private UserContext userContext;

    @Configuration
    static class Config {

        @Bean
        public UserContext userContext() {
            UserContext userContext = mock(UserContext.class);
            when(userContext.isUseSmartOutput()).thenReturn(Boolean.FALSE);
            return userContext;
        }

    }

    private BrowsableDirectoryField destination;
    private PdfDestinationPane victim;

    @Before
    public void setUp() {
        destination = spy(new BrowsableDirectoryField(false));
        victim = new PdfDestinationPane(destination, MODULE, userContext);
    }

    @Test
    public void setDestination() throws IOException {
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.getFootprint());
    }

    @Test
    @DirtiesContext
    public void setFallbackDestination() throws IOException {
        destination.getTextField().setText("");
        when(userContext.isUseSmartOutput()).thenReturn(Boolean.TRUE);
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestFallbackDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.getFootprint());
    }

    @Test
    public void dontSetFallbackDestinationIfFilled() throws IOException {
        destination.getTextField().setText("ChuckNorris");
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination).setTextFromFile(event.getFootprint());
    }

    @Test
    public void dontSetFallbackDestinationIfNoSmartOutput() throws IOException {
        File footprint = folder.newFile("test.pdf");
        SetDestinationRequest event = SetDestinationRequest.requestFallbackDestination(footprint, MODULE);
        victim.setDestination(event);
        verify(destination, never()).setTextFromFile(any());
    }
}
