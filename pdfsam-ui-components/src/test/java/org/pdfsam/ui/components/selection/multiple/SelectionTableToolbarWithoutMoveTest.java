/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14 dic 2015
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.AddButton;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.MoveDownButton;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.MoveUpButton;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class SelectionTableToolbarWithoutMoveTest {

    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);
    private SelectionTableToolbar victim;

    @Start
    public void start(Stage stage) {
        victim = new SelectionTableToolbar(MODULE, false);
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void moveDownIsMissing(FxRobot robot) {
        assertTrue(robot.lookup(b -> b instanceof AddButton).tryQuery().isPresent());
        assertTrue(robot.lookup(b -> b instanceof MoveUpButton).tryQuery().isEmpty());
    }

    @Test
    public void moveUpIsMissing(FxRobot robot) {
        assertTrue(robot.lookup(b -> b instanceof AddButton).tryQuery().isPresent());
        assertTrue(robot.lookup(b -> b instanceof MoveDownButton).tryQuery().isEmpty());
    }
}
