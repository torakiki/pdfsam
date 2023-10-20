package org.pdfsam.gui.components.content.preference;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/10/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.scene.Scene;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.StringPersistentProperty;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static java.util.Optional.of;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({ ApplicationExtension.class })
public class PreferencePrefixFieldTest {

    private final ApplicationContext appContext = mock(ApplicationContext.class);
    private final ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
        when(persistentSettings.get(eq(StringPersistentProperty.PREFIX))).thenReturn(of("prefix"));
        Scene scene = new Scene(new HBox(new PreferencePrefixField(appContext), new TextField()));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void prefixIsSet() {
        robot.clickOn("prefix").write("_something").type(KeyCode.TAB);
        verify(persistentSettings).set(StringPersistentProperty.PREFIX, "prefix_something");
    }
}