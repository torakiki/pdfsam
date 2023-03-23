/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
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
package org.pdfsam.gui.components.content.preference;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.StringPersistentProperty;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
@Tag("NoHeadless")
public class PreferenceBrowsableDirectoryFieldTest {

    @TempDir
    public Path folder;
    private final ApplicationContext appContext = mock(ApplicationContext.class);
    private final ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        PreferenceBrowsableDirectoryField victim = new PreferenceBrowsableDirectoryField(
                StringPersistentProperty.WORKING_PATH, appContext);
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validValue() throws Exception {
        var destination = Files.createTempDirectory(folder, null).toFile();
        typePathAndValidate(destination.getAbsolutePath());
        verify(persistentSettings).set(StringPersistentProperty.WORKING_PATH, destination.getAbsolutePath());
    }

    @Test
    public void invalidValue() throws Exception {
        var file = Files.createTempFile(folder, null, ".norris").toFile();
        typePathAndValidate(file.getAbsolutePath());
        verify(persistentSettings, never()).set(any(), any());
    }

    @Test
    public void emptyValue() {
        typePathAndValidate("");
        verify(persistentSettings).set(StringPersistentProperty.WORKING_PATH, "");
    }

    @Test
    public void blankValue() {
        typePathAndValidate("  ");
        verify(persistentSettings, never()).set(any(), any());
    }

    private void typePathAndValidate(String path) {
        robot.clickOn(".validable-container-field").write(path).push(KeyCode.TAB);
    }
}
