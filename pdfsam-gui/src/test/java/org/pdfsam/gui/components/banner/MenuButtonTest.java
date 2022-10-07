/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
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
package org.pdfsam.gui.components.banner;

import javafx.geometry.Side;
import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.injector.Injector;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class MenuButtonTest {

    private Injector injector;
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        injector = Injector.start(new MenuConfig());
        var menu = injector.instance(MenuButton.class);
        Scene scene = new Scene(menu);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() throws InterruptedException {
        var menu = injector.instance(AppContextMenu.class);
        robot.clickOn(".button");
        verify(menu).show(any(), eq(Side.BOTTOM), eq(0d), eq(0d));
    }
}
