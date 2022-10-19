/*
 * This file is part of the PDF Split And Merge source code
 * Created on 04/set/2014
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
package org.pdfsam.gui.components.dashboard;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.gui.test.DefaultPriorityDashboardItem;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadInitializeExtension.class, ClearEventStudioExtension.class })
public class DashboardItemPaneTest {

    @Test
    public void footerIsAdded() {
        var victim = new DashboardItemPane(new DefaultPriorityDashboardItem());
        assertNull(victim.lookup(".btn"));
        eventStudio().broadcast(new SetActiveToolRequest("any"));
        assertNotNull(victim.lookup(".btn"));
    }
}
