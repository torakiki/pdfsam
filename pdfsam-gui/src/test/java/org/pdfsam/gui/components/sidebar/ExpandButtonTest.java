/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ago/2014
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
package org.pdfsam.gui.components.sidebar;

import javafx.scene.Scene;
import javafx.scene.control.ToggleButton;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Isolated;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
@Isolated
public class ExpandButtonTest {

    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        app().persistentSettings().set(BooleanPersistentProperty.SIDEBAR_EXPANDED_STATE, true);
    }

    @Start
    public void start(Stage stage) {
        Scene scene = new Scene(new ExpandButton());
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void arrowIconRotates() {
        var toggle = robot.lookup(".sidebar-expand-toggle").queryAs(ToggleButton.class);
        var rotation = toggle.getRotate();
        robot.clickOn(".sidebar-expand-toggle");
        assertEquals((rotation + 180) % 360, toggle.getRotate());
    }
}
