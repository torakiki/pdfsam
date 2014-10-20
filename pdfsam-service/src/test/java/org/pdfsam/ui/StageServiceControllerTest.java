/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
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
package org.pdfsam.ui;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;

/**
 * @author Andrea Vacondio
 *
 */
public class StageServiceControllerTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    private StageServiceController victim;
    private StageService service;

    @Before
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

}
