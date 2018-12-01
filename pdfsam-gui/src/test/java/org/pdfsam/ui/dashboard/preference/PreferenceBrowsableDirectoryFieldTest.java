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
package org.pdfsam.ui.dashboard.preference;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.File;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferenceBrowsableDirectoryFieldTest extends ApplicationTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private UserContext userContext = mock(UserContext.class);

    @Override
    public void start(Stage stage) {
        PreferenceBrowsableDirectoryField victim = new PreferenceBrowsableDirectoryField(
                StringUserPreference.WORKING_PATH, userContext);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validValue() throws Exception {
        File destination = folder.newFolder();
        typePathAndValidate(destination.getAbsolutePath());
        verify(userContext).setStringPreference(StringUserPreference.WORKING_PATH, destination.getAbsolutePath());
    }

    @Test
    public void invalidValue() throws Exception {
        File file = folder.newFile("chuck.norris");
        typePathAndValidate(file.getAbsolutePath());
        verify(userContext, never()).setStringPreference(any(), any());
    }

    @Test
    public void emptyValue() {
        typePathAndValidate("");
        verify(userContext).setStringPreference(StringUserPreference.WORKING_PATH, "");
    }

    @Test
    public void blankValue() {
        typePathAndValidate("  ");
        verify(userContext, never()).setStringPreference(any(), any());
    }

    private void typePathAndValidate(String path) {
        clickOn(".validable-container-field").write(path).push(KeyCode.TAB);
    }
}
