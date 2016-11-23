/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
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
package org.pdfsam.premium;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
public class PremiumModulesControllerTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    private PremiumModulesService service;
    private PremiumModulesController victim;

    @Before
    public void setUp() {
        service = mock(PremiumModulesService.class);
        victim = new PremiumModulesController(service);
    }

    @Test
    public void fetchPremiumModules() {
        PremiumModule data = new PremiumModule();
        when(service.getPremiumModules()).thenReturn(Arrays.asList(data));

        Listener<PremiumModulesEvent> listener = mock(Listener.class);
        eventStudio().add(PremiumModulesEvent.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumModules();
        ArgumentCaptor<PremiumModulesEvent> captor = ArgumentCaptor.forClass(PremiumModulesEvent.class);
        verify(listener, timeout(1000).times(1)).onEvent(captor.capture());
        assertEquals(1, captor.getValue().premiumModules.size());
        assertEquals(data, captor.getValue().premiumModules.get(0));
    }

    @Test
    public void emptyFetchNews() {
        when(service.getPremiumModules()).thenReturn(Collections.emptyList());
        Listener<PremiumModulesEvent> listener = mock(Listener.class);
        eventStudio().add(PremiumModulesEvent.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumModules();
        verify(listener, after(1000).never()).onEvent(any());
    }

    @Test
    public void nullFetchNews() {
        when(service.getPremiumModules()).thenReturn(null);
        Listener<PremiumModulesEvent> listener = mock(Listener.class);
        eventStudio().add(PremiumModulesEvent.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumModules();
        verify(listener, after(1000).never()).onEvent(any());
    }

    @Test
    public void failingFetchNews() {
        when(service.getPremiumModules()).thenThrow(new RuntimeException());
        Listener<PremiumModulesEvent> listener = mock(Listener.class);
        eventStudio().add(PremiumModulesEvent.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumModules();
        verify(listener, after(1000).never()).onEvent(any());
    }
}
