/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.service.premium;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.premium.FetchPremiumModulesRequest;
import org.pdfsam.model.premium.PremiumTool;
import org.pdfsam.model.premium.PremiumToolsResponse;
import org.pdfsam.test.ClearEventStudioExtension;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ClearEventStudioExtension.class)
public class PremiumToolsControllerTest {

    private PremiumToolsService service;
    private PremiumToolsController victim;

    @BeforeEach
    public void setUp() {
        service = mock(PremiumToolsService.class);
        victim = new PremiumToolsController(service);
    }

    @Test
    public void fetchPremiumModules() {
        var data = new PremiumTool(1, "name", "desc", "url", "VISUAL");
        when(service.getPremiumTools()).thenReturn(List.of(data));

        Listener<PremiumToolsResponse> listener = mock(Listener.class);
        eventStudio().add(PremiumToolsResponse.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumTools();
        ArgumentCaptor<PremiumToolsResponse> captor = ArgumentCaptor.forClass(PremiumToolsResponse.class);
        verify(listener, timeout(1000).times(1)).onEvent(captor.capture());
        assertEquals(1, captor.getValue().premiumTools().size());
        assertEquals(data, captor.getValue().premiumTools().get(0));
    }

    @Test
    public void emptyFetchNews() {
        when(service.getPremiumTools()).thenReturn(Collections.emptyList());
        Listener<PremiumToolsResponse> listener = mock(Listener.class);
        eventStudio().add(PremiumToolsResponse.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumTools();
        verify(listener, after(1000).never()).onEvent(any());
    }

    @Test
    public void nullFetchNews() {
        when(service.getPremiumTools()).thenReturn(null);
        Listener<PremiumToolsResponse> listener = mock(Listener.class);
        eventStudio().add(PremiumToolsResponse.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumTools();
        verify(listener, after(1000).never()).onEvent(any());
    }

    @Test
    public void failingFetchNews() {
        when(service.getPremiumTools()).thenThrow(new RuntimeException());
        Listener<PremiumToolsResponse> listener = mock(Listener.class);
        eventStudio().add(PremiumToolsResponse.class, listener);
        victim.fetchPremium(FetchPremiumModulesRequest.INSTANCE);
        verify(service, timeout(1000).times(1)).getPremiumTools();
        verify(listener, after(1000).never()).onEvent(any());
    }
}
