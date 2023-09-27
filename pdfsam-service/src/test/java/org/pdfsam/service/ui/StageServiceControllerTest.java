/*
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.ui;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.lifecycle.CleanupRequest;
import org.pdfsam.model.ui.SetLatestStageStatusRequest;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.test.ClearEventStudioExtension;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ClearEventStudioExtension.class)
public class StageServiceControllerTest {

    private StageServiceController victim;
    private StageService service;

    @BeforeEach
    public void setUp() {
        this.service = mock(StageService.class);
        this.victim = new StageServiceController(service);
    }

    @Test
    public void requestStageStatus() {
        SetLatestStageStatusRequest event = new SetLatestStageStatusRequest(StageStatus.NULL);
        victim.requestStageStatus(event);
        verify(service).save(StageStatus.NULL);
    }

    @Test
    public void cleanup() {
        eventStudio().broadcast(new CleanupRequest());
        verify(service).clear();
    }

}
