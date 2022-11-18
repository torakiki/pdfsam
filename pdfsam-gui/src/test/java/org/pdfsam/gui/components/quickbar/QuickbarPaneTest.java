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
package org.pdfsam.gui.components.quickbar;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class QuickbarPaneTest {

    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        BaseQuickbarButtonsPane buttons = new BaseQuickbarButtonsPane();
        buttons.setId("buttons");
        Scene scene = new Scene(new QuickbarPane(buttons));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void click() {
        BaseQuickbarButtonsPane buttons = robot.lookup("#buttons").queryAs(BaseQuickbarButtonsPane.class);
        assertTrue(buttons.isDisplayText());
        robot.clickOn(".quickbar-expand-button");
        assertFalse(buttons.isDisplayText());
    }
}
