/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.split;

import java.util.function.Consumer;

import javafx.scene.control.Toggle;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.TaskParametersBuildStep;
import org.pdfsam.ui.support.Style;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

/**
 * Panel for the Split options
 * 
 * @author Andrea Vacondio
 *
 */
class SplitOptionsPane extends HBox implements TaskParametersBuildStep<SimpleSplitParameters> {

    private PredefinedSetOfPagesRadioButton burst = new PredefinedSetOfPagesRadioButton(PredefinedSetOfPages.ALL_PAGES,
            DefaultI18nContext.getInstance().i18n("Burst (Split into single pages)"));
    private PredefinedSetOfPagesRadioButton even = new PredefinedSetOfPagesRadioButton(PredefinedSetOfPages.EVEN_PAGES,
            DefaultI18nContext.getInstance().i18n("Split even pages"));
    private PredefinedSetOfPagesRadioButton odd = new PredefinedSetOfPagesRadioButton(PredefinedSetOfPages.ODD_PAGES,
            DefaultI18nContext.getInstance().i18n("Split odd pages"));

    private ToggleGroup group = new ToggleGroup();

    SplitOptionsPane() {
        super(20);
        burst.setToggleGroup(group);
        burst.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Explode the document into single pages")));
        burst.setSelected(true);
        even.setToggleGroup(group);
        even.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Split the document at every even page")));
        odd.setToggleGroup(group);
        odd.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Split the document at every odd page")));

        getStyleClass().addAll(Style.CONTAINER.css());
        getChildren().addAll(burst, even, odd);
    }

    SimpleSplitParameters createParams() {
        Toggle toggle = group.getSelectedToggle();
        if (toggle instanceof PredefinedSetOfPagesRadioButton) {
            return new SimpleSplitParameters(((PredefinedSetOfPagesRadioButton) toggle).getPages());
        }
        return null;
    }

    public void apply(SimpleSplitParameters params, Consumer<String> onError) {
        // nothing
    }
}
