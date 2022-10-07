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
package org.pdfsam.gui.components.dashboard.preference;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class PreferenceRadioButtonTest {

    private ApplicationContext appContext = mock(ApplicationContext.class);
    private ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
        Scene scene = new Scene(new HBox(
                new PreferenceRadioButton(BooleanPersistentProperty.SMART_OUTPUT, "select", false, appContext)));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void preferenceSetOnClick() {
        robot.clickOn("select");
        verify(persistentSettings).set(eq(BooleanPersistentProperty.SMART_OUTPUT), eq(Boolean.TRUE));
    }
}
