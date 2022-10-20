/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
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
package org.pdfsam.gui.components.dashboard.tools;

import javafx.scene.Node;
import javafx.scene.control.Labeled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.AdditionalDefaultPriorityTestTool;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.HighPriorityTestTool;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.test.LowPriorityTestTool;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class })
public class ToolsDashboardPaneTest {

    @Test
    public void priorityOrder() {
        var victim = new ToolsDashboardPane(
                Arrays.asList(new AdditionalDefaultPriorityTestTool(), new LowPriorityTestTool(),
                        new HighPriorityTestTool(), new DefaultPriorityTestTool()));
        Node title = victim.getChildren().stream().findFirst()
                .orElseThrow(() -> new NullPointerException("Unable to find the expected node"))
                .lookup(".dashboard-tools-tile-title");
        assertEquals("HighPriorityTestModule", ((Labeled) title).getText());
    }
}
