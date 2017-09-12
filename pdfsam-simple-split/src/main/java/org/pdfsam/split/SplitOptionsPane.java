/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
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
package org.pdfsam.split;

import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import java.util.Map;
import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.params.SinglePdfSourceMultipleOutputParametersBuilder;
import org.pdfsam.ui.commons.RadioButtonDrivenTextFieldsPane;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.model.parameter.AbstractSplitByPageParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

import javafx.scene.control.ComboBox;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.VBox;

/**
 * Panel for the Split options
 * 
 * @author Andrea Vacondio
 *
 */
class SplitOptionsPane extends VBox implements SplitParametersBuilderCreator, RestorableView {

    private ToggleGroup group = new ToggleGroup();
    private SplitAfterPredefinedSetOfPagesRadioButton splitAfterPredefined;
    private SplitAfterRadioButton splitAfter;
    private SplitByEveryRadioButton splitByEvery;

    SplitOptionsPane() {
        super(Style.DEFAULT_SPACING);
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> predefinedCombo = new ComboBox<>();
        predefinedCombo.getItems().add(KeyStringValueItem.keyValue(PredefinedSetOfPages.ALL_PAGES,
                DefaultI18nContext.getInstance().i18n("Every page")));
        predefinedCombo.getItems().add(KeyStringValueItem.keyValue(PredefinedSetOfPages.EVEN_PAGES,
                DefaultI18nContext.getInstance().i18n("Even pages")));
        predefinedCombo.getItems().add(KeyStringValueItem.keyValue(PredefinedSetOfPages.ODD_PAGES,
                DefaultI18nContext.getInstance().i18n("Odd pages")));
        splitAfterPredefined = new SplitAfterPredefinedSetOfPagesRadioButton(predefinedCombo);
        ValidableTextField splitAfterField = new ValidableTextField();
        splitAfter = new SplitAfterRadioButton(splitAfterField);
        ValidableTextField splitByEveryField = new ValidableTextField();
        splitByEvery = new SplitByEveryRadioButton(splitByEveryField);
        RadioButtonDrivenTextFieldsPane grid = new RadioButtonDrivenTextFieldsPane(group);

        splitAfterPredefined.setToggleGroup(group);
        splitAfter.setToggleGroup(group);
        splitByEvery.setToggleGroup(group);
        grid.addRow(splitAfterPredefined, predefinedCombo,
                helpIcon(DefaultI18nContext.getInstance().i18n("Split the document after the given page numbers")));
        grid.addRow(splitAfter, splitAfterField,
                helpIcon(DefaultI18nContext.getInstance().i18n("Split the document after the given page numbers")));
        grid.addRow(splitByEvery, splitByEveryField, helpIcon(DefaultI18nContext.getInstance()
                .i18n("Splits the PDF every \"n\" pages creating documents of \"n\" pages each")));
        splitAfterPredefined.setSelected(true);

        getStyleClass().addAll(Style.CONTAINER.css());
        getChildren().addAll(grid);
    }

    void setMaxPages(Integer value) {
        splitByEvery.setMaxPages(value);
    }

    @Override
    public SinglePdfSourceMultipleOutputParametersBuilder<? extends AbstractSplitByPageParameters> getBuilder(
            Consumer<String> onError) {
        return ((SplitParametersBuilderCreator) group.getSelectedToggle()).getBuilder(onError);
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        splitAfterPredefined.saveStateTo(data);
        splitAfter.saveStateTo(data);
        splitByEvery.saveStateTo(data);
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        splitAfterPredefined.restoreStateFrom(data);
        splitAfter.restoreStateFrom(data);
        splitByEvery.restoreStateFrom(data);
    }
}
