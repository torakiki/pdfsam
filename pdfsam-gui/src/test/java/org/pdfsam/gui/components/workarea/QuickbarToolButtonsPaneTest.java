/*
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ago/2014
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
package org.pdfsam.gui.components.workarea;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.test.LowPriorityTestTool;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class QuickbarToolButtonsPaneTest {

    private QuickbarToolButtonsPane victim;

    @BeforeEach
    public void setUp() {
        victim = new QuickbarToolButtonsPane(List.of(new DefaultPriorityTestTool(), new LowPriorityTestTool()));
    }

    @Test
    public void onSetCurrentModuleRequest() {
        var buttons = victim.getChildren().stream().filter(t -> t instanceof ToolButton).map(t -> (ToolButton) t)
                .toList();
        assertEquals(2, buttons.size());

        buttons.forEach(tool -> assertFalse(tool.isSelected()));
        var r = new SetActiveToolRequest(DefaultPriorityTestTool.ID);
        victim.onSetCurrentModuleRequest(r);
        assertTrue(buttons.stream().filter(ToolButton::isSelected)
                .allMatch(m -> m.toolBinding().equals(DefaultPriorityTestTool.ID)));
        assertTrue(buttons.stream().filter(m -> !m.toolBinding().equals(DefaultPriorityTestTool.ID))
                .noneMatch(ToolButton::isSelected));
    }

    @Test
    public void displayTest() {
        var buttons = victim.getChildren().stream().filter(t -> t instanceof ToolButton).map(t -> (ToolButton) t)
                .toList();
        assertEquals(2, buttons.size());
        buttons.forEach(m -> assertFalse(m.isDisplayText()));
        victim.setDisplayText(true);
        buttons.forEach(m -> assertTrue(m.isDisplayText()));
    }
}
