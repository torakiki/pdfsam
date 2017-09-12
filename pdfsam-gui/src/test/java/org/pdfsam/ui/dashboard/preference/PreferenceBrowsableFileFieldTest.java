/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;

import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class PreferenceBrowsableFileFieldTest extends GuiTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private UserContext userContext = mock(UserContext.class);

    @Override
    protected Parent getRootNode() {
        PreferenceBrowsableFileField victim = new PreferenceBrowsableFileField(StringUserPreference.WORKING_PATH,
                FileType.PDF, OpenType.OPEN, userContext);
        victim.setId("victim");
        return victim;
    }

    @Test
    public void validValue() throws Exception {
        File file = folder.newFile("chuck.pdf");
        typePathAndValidate(file.getAbsolutePath());
        verify(userContext).setStringPreference(StringUserPreference.WORKING_PATH, file.getAbsolutePath());
    }

    @Test
    public void invalidValue() throws Exception {
        File file = folder.newFile("chuck.norris");
        typePathAndValidate(file.getAbsolutePath());
        verify(userContext, never()).setStringPreference(any(), any());
    }

    @Test
    public void emptyValue() throws Exception {
        typePathAndValidate("");
        verify(userContext).setStringPreference(StringUserPreference.WORKING_PATH, "");
    }

    @Test
    public void blankValue() throws Exception {
        typePathAndValidate("  ");
        verify(userContext, never()).setStringPreference(any(), any());
    }

    private void typePathAndValidate(String path) throws Exception {
        ValidableTextField field = find(".validable-container-field");
        // TODO replace with typing when slash works
        FXTestUtils.invokeAndWait(() -> field.setText(path), 2);
        click(field).type(KeyCode.TAB);
    }

}
