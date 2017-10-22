/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.update;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */

public class UpdatesControllerTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeJavaFxThreadRule javaFX = new InitializeJavaFxThreadRule();
    private UpdatesController victim;
    private UpdateService service;
    private Listener<UpdateAvailableEvent> listener;

    @Before
    public void setUp() {
        service = mock(UpdateService.class);
        listener = mock(Listener.class);
        Pdfsam pdfsam = mock(Pdfsam.class);
        when(pdfsam.property(ConfigurableProperty.VERSION)).thenReturn("3.0.0.M1");
        victim = new UpdatesController(service, pdfsam);
    }

    @Test
    public void pasitiveCheckForUpdates() {
        when(service.getLatestVersion()).thenReturn("3.0.0");
        eventStudio().add(UpdateAvailableEvent.class, listener);
        victim.checkForUpdates(UpdateCheckRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getLatestVersion();
        verify(listener, timeout(1000).times(1)).onEvent(any(UpdateAvailableEvent.class));
    }

    @Test
    public void negativeCheckForUpdates() {
        when(service.getLatestVersion()).thenReturn("3.0.0.M1");
        eventStudio().add(UpdateAvailableEvent.class, listener);
        victim.checkForUpdates(UpdateCheckRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getLatestVersion();
        verify(listener, after(1000).never()).onEvent(any(UpdateAvailableEvent.class));
    }

    @Test
    public void exceptionalCheckForUpdates() {
        when(service.getLatestVersion()).thenThrow(new RuntimeException("Mock"));
        eventStudio().add(UpdateAvailableEvent.class, listener);
        victim.checkForUpdates(UpdateCheckRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getLatestVersion();
        verify(listener, after(1000).never()).onEvent(any(UpdateAvailableEvent.class));
    }
}
