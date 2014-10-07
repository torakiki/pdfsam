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

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.prefs.Preferences;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.jr.ob.JSON;
import com.fasterxml.jackson.jr.ob.JSONObjectException;

/**
 * @author Andrea Vacondio
 *
 */
public class StatefullPreferencesStageStatusServiceTest {
    private StatefullPreferencesStageStatusService victim = new StatefullPreferencesStageStatusService();

    @After
    @Before
    public void clear() {
        victim.clearStageStatus();
    }

    @Test
    public void save() throws JSONObjectException, IOException {
        StageStatus status = new StageStatus(10, 20, 100, 200, StageMode.ICONIFIED);
        victim.save(status);
        StageStatus storedStatus = JSON.std.beanFrom(
                StageStatus.class,
                Preferences.userRoot().node(StatefullPreferencesStageStatusService.STAGE_PATH)
                        .get(StatefullPreferencesStageStatusService.STAGE_STATUS_KEY, ""));
        assertEquals(status, storedStatus);
    }

    @Test
    public void testClear() {
        StageStatus status = new StageStatus(10, 20, 100, 200, StageMode.ICONIFIED);
        victim.save(status);
        victim.clearStageStatus();
        assertTrue(isBlank(Preferences.userRoot().node(StatefullPreferencesStageStatusService.STAGE_PATH)
                .get(StatefullPreferencesStageStatusService.STAGE_STATUS_KEY, "")));
    }

    @Test
    public void nullLatest() {
        assertEquals(StageStatus.NULL, victim.getLatestStatus());
    }

    @Test
    public void getLatest() {
        StageStatus status = new StageStatus(10, 20, 100, 200, StageMode.ICONIFIED);
        victim.save(status);
        victim.flush();
        assertEquals(status, victim.getLatestStatus());
    }
}
